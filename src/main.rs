// Impack, a tool for interactively sorting images.

// Copyright © 2023 Matthew Rothlisberger
// SPDX-License-Identifier: GPL-3.0-only

// This program is free software: you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation, version 3 of the License
// only.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program (in the LICENSES directory). If not, see
// <https://www.gnu.org/licenses/>.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/main.rs

// <>

use std::{
    env,
    fs::{self, File},
    io::Write,
    num::NonZeroU32,
    path::PathBuf,
    rc::Rc,
};

use ab_glyph::FontRef;
use rfd::FileDialog;
use winit::{
    dpi::PhysicalPosition,
    event::{ElementState, Event, KeyEvent, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    keyboard::{Key, NamedKey},
    window::WindowBuilder,
};
use zip::{write::FileOptions, ZipWriter};

#[derive(Debug)]
enum State {
    Init,
    Elect,
    DspnCat,
    DspnPath,
    DspnCom,
}

enum Dspn {
    List,
    Dir,
    Zip,
}

enum Rot {
    R0,
    R90,
    R180,
    R270,
}

/// Mosaic maximum edge length
const MSC_MX_E: u32 = 200;

const CFG_REL_PATH: &'static str = ".config/impack/impack.cfg";

const DEF_BACK_COL: u32 = 0xFFFFFF;
const DEF_TEXT_COL: u32 = 0xFF0000;

const DEF_FONT_PATH: &'static str = "FreeMono.otf";
const DEF_TRAIL_PATH: &'static str = ".impack-trail";

const HELP: &'static str = "Impack, a tool for interactively sorting images

Usage: impack [<path to source dir> [<path to output dir>]]

Reads images from a directory and presents them one at a time. Press a
letter key to categorize the image or press space to skip. Press left
and right arrow keys to rotate. Once all images are seen, choose to
output categories as lists, directories of copied images, or
compressed archives of the same.";

macro_rules! config_expand {
    ( ) => {};
}

struct Cfg {
    back_col: u32,
    text_col: u32,
    font_path: PathBuf,
    trail_path: PathBuf,
}

fn parse_config(text: String) -> Cfg {
    let (mut bcol, mut tcol, mut fpat, mut tpat) = (None, None, None, None);

    for mut ent in text.lines().filter_map(|l| {
        if l.is_empty() || l.starts_with('#') {
            None
        } else {
            Some(l.split_ascii_whitespace())
        }
    }) {
        match ent.next() {
            Some(h) => match h {
                "BackColor" => bcol = ent.next(),
                "TextColor" => tcol = ent.next(),
                "FontPath" => fpat = ent.next(),
                "TrailPath" => tpat = ent.next(),
                _ => panic!(),
            },
            None => unreachable!(),
        }
    }

    Cfg {
        back_col: match bcol {
            Some(cs) => color_parse(cs.trim().as_bytes()),
            None => DEF_BACK_COL,
        },
        text_col: match tcol {
            Some(cs) => color_parse(cs.trim().as_bytes()),
            None => DEF_TEXT_COL,
        },
        font_path: match fpat {
            Some(p) => PathBuf::from(p),
            None => DEF_FONT_PATH.into(),
        },
        trail_path: match tpat {
            Some(p) => PathBuf::from(p),
            None => DEF_TRAIL_PATH.into(),
        },
    }
}

fn color_parse(text: &[u8]) -> u32 {
    let mut acc: u32 = 0;
    for b in text {
        match b {
            b'0'..=b'9' => {
                acc *= 0x10;
                acc += (b - b'0') as u32;
            }
            b'A'..=b'F' => {
                acc *= 0x10;
                acc += 0xA + (b - b'A') as u32;
            }
            b'a'..=b'f' => {
                acc *= 0x10;
                acc += 0xA + (b - b'a') as u32;
            }
            _ => panic!(),
        }
    }

    acc
}

fn main() {
    //! Specification: Accept a directory containing images. Display
    //! the images one by one; accept a keystroke to assign each image
    //! to a category. When finished with all images, report out lists
    //! of images from each category and ask for an action (copy out as
    //! directory, move out as directory, pack into archive).

    // TODO: Add frame icon for graphical managers like KDE or Windows
    // TODO: Plenty of range to select one's own options and keybinds
    // TODO: Draw functions with less argument duplication
    // TODO: Scroll capability in disposition view

    let mut state = State::Init;

    let cfg_path = PathBuf::from(std::env::var("HOME").unwrap()).join(CFG_REL_PATH);
    let cfg_content = match std::fs::read_to_string(&cfg_path) {
        Ok(s) => s,
        Err(e) => panic!("{}", e),
    };
    let config = parse_config(cfg_content);

    let mut inp_path = env::args().nth(1).map(|s| PathBuf::from(s));
    let mut out_path = env::args().nth(2).map(|s| PathBuf::from(s));

    let mut trail_out = File::options()
        .write(true)
        .create(true)
        .open(config.trail_path)
        .unwrap();

    let mut file_paths: Vec<PathBuf> = vec![];
    let mut cats_assigned: Vec<Option<u8>> = vec![];
    let mut useful_minis: Vec<Option<image::DynamicImage>> = vec![];

    // max of 26 elements
    let mut cats: Vec<Vec<PathBuf>> = vec![Vec::new(); 26];
    let mut mosaics: Vec<Vec<image::DynamicImage>> = vec![Vec::new(); 26];

    let mut dspns: Vec<(u8, Dspn)> = vec![];

    let mut spos = 0;

    let mut cur_fp = 0;
    let mut cur_cat = 0;

    let mut cur_rot = Rot::R0;

    let mut loaded_image: Option<(usize, image::DynamicImage)> = None;

    let event_loop = EventLoop::new().unwrap();
    let frame = Rc::new(WindowBuilder::new().build(&event_loop).unwrap());

    event_loop.set_control_flow(ControlFlow::Wait);

    let context = softbuffer::Context::new(frame.clone()).unwrap();
    let mut surface = softbuffer::Surface::new(&context, frame.clone()).unwrap();
    let mut full_buffer = vec![0u32; 0];

    let mut no_mods = true;

    // TODO: allow font path setting to change font used
    let font = FontRef::try_from_slice(include_bytes!("../FreeMono.otf")).unwrap();

    event_loop.run(move |event, elwt| match event {
        Event::WindowEvent {
            window_id: _,
            event: WindowEvent::RedrawRequested,
        } => {
            let (width, height) = {
                let size = frame.inner_size();
                (size.width, size.height)
            };

            let mut fullheight = height;

            surface.resize(
                NonZeroU32::new(width).unwrap(),
                NonZeroU32::new(height).unwrap(),
            ).unwrap();

            let mut draw_buffer = surface.buffer_mut().unwrap();

            full_buffer.clear();
            full_buffer.extend_from_slice(&vec![config.back_col; (width * height) as usize]);

            // println!("State: {:?}", state);

            draw::clear(&mut draw_buffer);

            match state {
                State::Init => {
                    while inp_path.is_none() {
                        draw::ins_text(
                            &font,
                            &mut full_buffer,
                            (width, height),
                            (0, 0),
                            80,
                            config.text_col,
                            b"Choose input directory...",
                        );

                        inp_path = FileDialog::new().pick_folder();
                    }

                    let files = fs::read_dir(inp_path.as_ref().unwrap()).unwrap();

                    files.filter_map(|fr| {
                        let fp = fr.unwrap().path();
                        if fp.is_file() {
                            Some(fp)
                        } else {
                            None
                        }
                    }).for_each(|f| {
                        file_paths.push(f);
                        cats_assigned.push(None);
                        useful_minis.push(None);
                    });

                    file_paths.sort();

                    state = State::Elect;
                    frame.request_redraw();

                }
                State::Elect => {
                    let scaled_image = {
                        let filect = file_paths.len();
                        if cur_fp >= filect {
                            for i in 0..filect {
                                match cats_assigned[i] {
                                    Some(c) => {
                                        cats[c as usize].push(file_paths[i].clone());
                                        mosaics[c as usize].push(useful_minis[i].take().unwrap());
                                    }
                                    None => (),
                                }
                            }

                            println!("Categories: {:?}", cats);

                            state = State::DspnCat;
                            frame.request_redraw();
                            return;
                        }

                        let orig_image = match loaded_image {
                            Some((f, ref img)) if f == cur_fp => img,
                            _ => match image::ImageReader::open(&file_paths[cur_fp])
                                .unwrap()
                                .with_guessed_format()
                                .unwrap()
                                .decode()
                            {
                                Ok(im) => {
                                    loaded_image = Some((cur_fp, im));
                                    if let Some((_, ref img)) = loaded_image {
                                        img
                                    } else {
                                        unreachable!()
                                    }
                                }
                                Err(_) => {
                                    eprintln!("Skipped file: {:?}", file_paths[cur_fp]);

                                    file_paths.remove(cur_fp);
                                    cats_assigned.remove(cur_fp);
                                    useful_minis.remove(cur_fp);

                                    frame.request_redraw();
                                    return;
                                }
                            }
                        };

                        let rot_image = match cur_rot {
                            Rot::R0 => orig_image,
                            Rot::R90 => &orig_image.rotate90(),
                            Rot::R180 => &orig_image.rotate180(),
                            Rot::R270 => &orig_image.rotate270(),
                        };

                        let (iw, ih) = (rot_image.width(), rot_image.height());

                        if iw > width || ih > height {
                            rot_image.resize(width, height, image::imageops::Triangle)
                        } else {
                            rot_image.clone()
                        }
                    };

                    let (siw, sih) = (scaled_image.width(), scaled_image.height());
                    let (xofs, yofs) = ((width - siw) / 2, (height - sih) / 2);

                    let img_pix_buf = scaled_image.into_rgb8().into_raw();

                    draw::ins_image(&mut draw_buffer, (width, height), (xofs, yofs), (siw, sih), &img_pix_buf);

                    draw::ins_text(
                        &font,
                        &mut draw_buffer,
                        (width, height),
                        (0, (height - height / 20)),
                        (height / 20) as u8,
                        config.text_col,
                        format!("{cur_fp}").as_bytes(),
                    );
                }

                // Disposition Selection
                // - Display category and its image contents
                // - Ask for desired disposition: list or copy or zip or nothing
                // - Ask for target directory
                State::DspnCat => {
                    loop {
                        if cur_cat >= cats.len() {
                            state = State::DspnPath;
                            frame.request_redraw();
                            return;
                        }

                        if cats[cur_cat].is_empty() {
                            cur_cat += 1;
                        } else {
                            break;
                        }
                    }

                    let mut topline: u32 = 0;
                    topline += 40 * draw::flow_text(
                        &font,
                        &mut full_buffer,
                        (width, height),
                        (0, topline),
                        None,
                        40,
                        config.text_col,
                        format!("Dispose Category {}", (cur_cat as u8 + 0x41) as char).as_bytes(),
                    );

                    if topline >= fullheight - 100 {
                        full_buffer.extend_from_slice(&vec![config.back_col; width as usize * 100]);
                        fullheight += 100;
                    }

                    // display selected image mosaic
                    let (mut imx, mut imy) = (0, topline + 20);
                    let mut crossc = 0;
                    for im in mosaics[cur_cat].iter() {
                        let imb = im.to_rgb8();
                        let imd = imb.dimensions();

                        let (xofs, yofs) = ((MSC_MX_E - imd.0) / 2, (MSC_MX_E - imd.1) / 2);

                        draw::ins_image(&mut full_buffer, (width, height), (imx + xofs, imy + yofs), imd, &imb);

                        if crossc <= 2 {
                            imx += MSC_MX_E;
                            crossc += 1;
                        } else {
                            imx = 0;
                            imy += MSC_MX_E;
                            crossc = 0;
                        }

                        if imy >= fullheight - 100 {
                            full_buffer.extend_from_slice(&vec![config.back_col; width as usize * 100]);
                            fullheight += 100;
                        }
                    }

                    topline = imy + MSC_MX_E;

                    // list file names
                    topline += 20;
                    for fp in cats[cur_cat].iter() {
                        draw::ins_text(
                            &font,
                            &mut full_buffer,
                            (width, height),
                            (0, topline),
                            20,
                            config.text_col,
                            fp.file_name().unwrap().as_encoded_bytes(),
                        );
                        topline += 20;

                        if topline >= fullheight - 100 {
                            full_buffer.extend_from_slice(&vec![config.back_col; width as usize * 100]);
                            fullheight += 100;
                        }
                    }

                    topline += 20;
                    draw::flow_text(
                        &font,
                        &mut full_buffer,
                        (width, height),
                        (0, topline),
                        None,
                        30,
                        config.text_col,
                        b"Disposition options:\n[L]ist files\n[C]opy to directory\n[Z]ip into archive\n[N]othing"
                    );

                    let mut sect_start = spos as usize * width as usize;
                    let sect_span = height as usize * width as usize;
                    if sect_start + sect_span >= full_buffer.len() {
                        sect_start = full_buffer.len() - sect_span
                    }
                    draw_buffer.copy_from_slice(&full_buffer[sect_start..sect_start + sect_span]);

                }
                State::DspnPath => {
                    while !dspns.is_empty() && out_path.is_none() {
                        draw::ins_text(
                            &font,
                            &mut draw_buffer,
                            (width, height),
                            (0, 0),
                            80,
                            config.text_col,
                            b"Choose output directory...",
                        );

                        out_path = FileDialog::new().set_directory(inp_path.as_ref().unwrap()).pick_folder();
                    }

                    state = State::DspnCom;
                    spos = 0;
                    frame.request_redraw();
                }
                State::DspnCom => {
                    if dspns.is_empty() {
                        elwt.exit();
                        return;
                    }

                    let mut topline: u32 = 0;

                    topline += 50 + 50 * draw::flow_text(
                        &font,
                        &mut draw_buffer,
                        (width, height),
                        (0, topline.saturating_sub(spos)),
                        None,
                        50,
                        config.text_col,
                        format!("Output directory: {:?}", out_path.as_ref().unwrap()).as_bytes(),
                    );

                    if topline >= fullheight - 100 {
                        full_buffer.extend_from_slice(&vec![config.back_col; width as usize * 100]);
                        fullheight += 100;
                    }

                    // TODO: above path must print without backslashes on Windows

                    for (c, d) in &dspns {
                        let cat = &cats[*c as usize];
                        let cat_char = (c + 0x41) as char;

                        // name
                        draw::ins_text(
                            &font,
                            &mut full_buffer,
                            (width, height),
                            (0, topline.saturating_sub(spos)),
                            30,
                            config.text_col,
                            format!("Category {}", cat_char).as_bytes(),
                        );
                        topline += 30;

                        // count
                        draw::ins_text(
                            &font,
                            &mut full_buffer,
                            (width, height),
                            (0, topline.saturating_sub(spos)),
                            30,
                            config.text_col,
                            format!("{} images", cat.len()).as_bytes(),
                        );
                        topline += 30;

                        // disp
                        draw::ins_text(
                            &font,
                            &mut full_buffer,
                            (width, height),
                            (0, topline.saturating_sub(spos)),
                            30,
                            config.text_col,
                            format!("To {}", match d {
                                Dspn::List => "list",
                                Dspn::Dir => "subfolder",
                                Dspn::Zip => "archive"
                            }).as_bytes(),
                        );
                        topline += 60;

                        if topline >= fullheight - 100 {
                            full_buffer.extend_from_slice(&vec![config.back_col; width as usize * 100]);
                            fullheight += 100;
                        }
                    }

                    draw::ins_text(
                        &font,
                        &mut full_buffer,
                        (width, height),
                        (0, topline.saturating_sub(spos)),
                        40,
                        config.text_col,
                        b"Press <Enter> to commit..."
                    );

                    let mut sect_start = spos as usize * width as usize;
                    let sect_span = height as usize * width as usize;
                    if sect_start + sect_span >= full_buffer.len() {
                        sect_start = full_buffer.len() - sect_span
                    }
                    draw_buffer.copy_from_slice(&full_buffer[sect_start..sect_start + sect_span]);
                }
            }

            draw_buffer.present().unwrap();
        }
        Event::WindowEvent {
            window_id: _,
            event: WindowEvent::ModifiersChanged(mods),
        } => {
            let mstat = mods.state();
            no_mods = mstat.is_empty()
                || (!mstat.control_key() && !mstat.alt_key() && !mstat.super_key());
        }
        Event::WindowEvent {
            window_id: _,
            event: WindowEvent::KeyboardInput {
                device_id: _,
                event:
                KeyEvent {
                    logical_key: lokey,
                    state: ElementState::Released,
                    repeat: false,
                    ..
                },
                is_synthetic: _,
            },
        } => {
            match lokey {
                Key::Named(NamedKey::ArrowUp) => match state {
                    State::DspnCat | State::DspnCom => {
                        spos = spos.saturating_sub(100);
                        frame.request_redraw();
                    }
                    _ => (),
                }
                Key::Named(NamedKey::ArrowDown) => match state {
                    State::DspnCat | State::DspnCom => {
                        spos += 100;
                        frame.request_redraw();
                    }
                    _ => (),
                }
                Key::Named(NamedKey::ArrowLeft) => match state {
                    State::Elect => {
                        cur_rot = match cur_rot {
                            Rot::R0 => Rot::R270,
                            Rot::R90 => Rot::R0,
                            Rot::R180 => Rot::R90,
                            Rot::R270 => Rot::R180,
                        };
                        frame.request_redraw();
                    }
                    _ => (),
                }
                Key::Named(NamedKey::ArrowRight) => match state {
                    State::Elect => {
                        cur_rot = match cur_rot {
                            Rot::R0 => Rot::R90,
                            Rot::R90 => Rot::R180,
                            Rot::R180 => Rot::R270,
                            Rot::R270 => Rot::R0,
                        };
                        frame.request_redraw();
                    }
                    _ => (),
                }
                Key::Named(NamedKey::Space) => match state {
                    State::Elect => {
                        if cats_assigned[cur_fp].is_some() {
                            cats_assigned[cur_fp] = None;
                            useful_minis[cur_fp] = None;

                            trail_out.write_all(format!("{:?} ><\n", file_paths[cur_fp]).as_bytes()).unwrap()
                        }
                        cur_fp += 1;
                        cur_rot = Rot::R0;
                        frame.request_redraw();
                    }
                    _ => (),
                }
                Key::Named(NamedKey::Backspace) => match state {
                    State::Elect => if cur_fp > 0 {
                        cur_fp -= 1;
                        frame.request_redraw();
                    }
                    State::DspnCat => if cur_cat > 0 {
                        cur_cat -= 1;
                        if let Some((end_cat, _)) = dspns.last() {
                            if *end_cat as usize == cur_cat {
                                dspns.pop();
                            }
                        }
                        frame.request_redraw();
                    }
                    _ => (),
                }
                Key::Named(NamedKey::Enter) => match state {
                    State::DspnCom => {
                        let path = out_path.clone().unwrap();
                        for (c, d) in &dspns {
                            let cat = &cats[*c as usize];
                            let cat_char = (c + 0x41) as char;
                            match d {
                                Dspn::List => {
                                    let name = format!("impack-list-{cat_char}.txt");
                                    let mut out = File::create(path.join(name)).unwrap();

                                    for fp in cat {
                                        out.write_all(fp.file_name().unwrap().as_encoded_bytes())
                                            .unwrap();
                                        out.write_all(&[b'\n']).unwrap();
                                    }
                                }
                                Dspn::Dir => {
                                    let name = format!("impack-dir-{cat_char}");
                                    let out_path = path.join(name);

                                    fs::create_dir(&out_path).unwrap();

                                    for fp in cat {
                                        let cploc = out_path.join(fp.file_name().unwrap());
                                        fs::copy(fp, cploc).unwrap();
                                    }
                                }
                                Dspn::Zip => {
                                    let name = format!("impack-arcv-{cat_char}.zip");
                                    let mut out =
                                        ZipWriter::new(File::create(path.join(name)).unwrap());

                                    let options = FileOptions::default()
                                        .compression_method(zip::CompressionMethod::Deflated);

                                    for fp in cat {
                                        let img_cts = fs::read(fp).unwrap();
                                        out.start_file(
                                            fp.file_name().unwrap().to_string_lossy(),
                                            options,
                                        ).unwrap();
                                        out.write_all(&img_cts).unwrap();
                                    }
                                    out.finish().unwrap();
                                }
                            }
                        }
                        elwt.exit()
                    }
                    _ => (),
                }

                Key::Character(kpstr) => match state {
                    State::Elect => {
                        if no_mods && kpstr.len() == 1 {
                            let cbyt = kpstr.as_bytes()[0];
                            if cbyt.is_ascii_alphabetic() {
                                cats_assigned[cur_fp] = Some(cbyt - if cbyt.is_ascii_uppercase() { 0x41 } else { 0x61 });
                                useful_minis[cur_fp] = Some(loaded_image.as_ref().unwrap().1.thumbnail(MSC_MX_E, MSC_MX_E));
                                trail_out.write_all(format!("{:?} >> {}\n",
                                                            file_paths[cur_fp],
                                                            String::from_utf8_lossy(&[cbyt.to_ascii_uppercase()])).as_bytes())
                                    .unwrap();
                                cur_fp += 1;
                                frame.request_redraw();
                            }
                        }
                    }
                    State::DspnCat => {
                        if no_mods && kpstr.len() == 1 {
                            let cbyt = kpstr.as_bytes()[0];

                            match cbyt.to_ascii_uppercase() {
                                b'L' => dspns.push((cur_cat as _, Dspn::List)),
                                b'C' => dspns.push((cur_cat as _, Dspn::Dir)),
                                b'Z' => dspns.push((cur_cat as _, Dspn::Zip)),
                                b'N' => (),
                                _ => return,
                            }

                            cur_cat += 1;
                            frame.request_redraw();
                        }
                    }
                    _ => (),
                }

                _ => (),
            }
        }
        Event::WindowEvent { window_id: _, event: WindowEvent::MouseWheel { device_id: _, delta, phase: _ } } => {
            let amount: i32 = match delta {
                winit::event::MouseScrollDelta::LineDelta(_, l) => (l * 20.0) as i32,
                winit::event::MouseScrollDelta::PixelDelta(PhysicalPosition{y, ..}) => y as i32,
            };
            if amount < 0 {
                spos = spos.saturating_sub(amount.abs() as u32)
            } else {
                spos += amount as u32;
            }
            frame.request_redraw();
        }
        Event::WindowEvent {
            window_id: _,
            event: WindowEvent::CloseRequested,
        } => elwt.exit(),
        _ => (),
    }).unwrap();
}

mod draw {
    use ab_glyph::{Font, FontRef, ScaleFont};

    fn merge_col(bg: u32, fg: u32, c: u8) -> u32 {
        let (bg_r, bg_g, bg_b) = ((bg >> 16 & 0xFF), (bg >> 8 & 0xFF), (bg & 0xFF));
        let (fg_r, fg_g, fg_b) = ((fg >> 16 & 0xFF), (fg >> 8 & 0xFF), (fg & 0xFF));

        let final_r =
            (fg_r * c as u32) / u8::MAX as u32 + bg_r * (u8::MAX - c) as u32 / u8::MAX as u32;
        let final_g =
            (fg_g * c as u32) / u8::MAX as u32 + bg_g * (u8::MAX - c) as u32 / u8::MAX as u32;
        let final_b =
            (fg_b * c as u32) / u8::MAX as u32 + bg_b * (u8::MAX - c) as u32 / u8::MAX as u32;

        (final_r.min(u8::MAX as u32) << 16)
            + (final_g.min(u8::MAX as u32) << 8)
            + final_b.min(u8::MAX as u32)
    }

    pub fn ins_text(
        font: &FontRef,
        buf: &mut dyn std::ops::DerefMut<Target = [u32]>,
        dim: (u32, u32),
        pos: (u32, u32),
        pxh: u8,
        col: u32,
        string: &[u8],
    ) {
        let (pxh, pxw) = (pxh as u32, (pxh as u32 * 3) / 5);
        let hadv = font.as_scaled(pxh as f32).h_advance(font.glyph_id('a')) as u32;

        for (i, b) in string.iter().enumerate() {
            let glyph = font.glyph_id(*b as char).with_scale(pxh as f32);
            if let Some(outline) = font.outline_glyph(glyph) {
                outline.draw(|x, y, c| {
                    let head = dim.0 * (pos.1 + y) + pos.0 + x + hadv * i as u32 + 20;
                    let cov = ((c * 255.0) as u32).min(u8::MAX as u32) as u8;
                    let out = merge_col(buf[head as usize], col, cov);
                    if cov > 0 {
                        buf[head as usize] = out;
                    }
                })
            }
        }
    }

    pub fn flow_text(
        font: &FontRef,
        buf: &mut dyn std::ops::DerefMut<Target = [u32]>,
        dim: (u32, u32),
        pos: (u32, u32),
        maxw: Option<u32>,
        pxh: u8,
        col: u32,
        string: &[u8],
    ) -> u32 {
        let hadv = font.as_scaled(pxh as f32).h_advance(font.glyph_id('a')) as u32;

        let pxw = (pxh as u32 * 3) / 5;

        let avail = dim.0 - pos.0;
        let maxw = match maxw {
            Some(m) => m.min(avail),
            None => avail,
        };

        let maxc = maxw / hadv;
        let lines = 1 + (string.len() / maxc as usize);

        let mut ll = lines;
        let mut curh = pos.1;
        let mut stac: usize = 0;

        loop {
            let lasc = stac + (maxc as usize).min(string.len() - stac);
            ins_text(font, buf, dim, (pos.0, curh), pxh, col, &string[stac..lasc]);

            if ll > 1 {
                ll -= 1;
                curh += pxh as u32;
                stac += maxc as usize;
                continue;
            }

            break;
        }

        lines as _
    }

    pub fn ins_image(
        buf: &mut dyn std::ops::DerefMut<Target = [u32]>,
        bdim: (u32, u32),
        pos: (u32, u32),
        idim: (u32, u32),
        pixels: &[u8],
    ) {
        for iy in 0..idim.1 {
            for ix in 0..idim.0 {
                let ip = 3 * (idim.0 * iy + ix) as usize;
                let op = (pos.1 + iy) * bdim.0 + pos.0 + ix;

                let pxl = ((pixels[ip] as u32) << 16)
                    + ((pixels[ip + 1] as u32) << 8)
                    + pixels[ip + 2] as u32;

                let ln = buf.len();
                buf[(op as usize).min(ln - 1)] = pxl;
            }
        }
    }

    pub fn clear(buf: &mut dyn std::ops::DerefMut<Target = [u32]>) {
        for p in 0..buf.len() {
            buf[p] = (255 << 16) + (255 << 8) + 255
        }
    }
}
