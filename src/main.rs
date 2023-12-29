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

use rfd::FileDialog;
use winit::{
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

/// Mosaic maximum edge length
const MSC_MX_E: u32 = 200;

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

    let mut inp_path = env::args().nth(1).map(|s| PathBuf::from(s));
    let mut out_path = env::args().nth(2).map(|s| PathBuf::from(s));

    let mut file_paths: Vec<PathBuf> = vec![];
    let mut cats_assigned: Vec<Option<u8>> = vec![];
    let mut useful_minis: Vec<Option<image::DynamicImage>> = vec![];

    // max of 26 elements
    let mut cats: Vec<Vec<PathBuf>> = vec![Vec::new(); 26];
    let mut mosaics: Vec<Vec<image::DynamicImage>> = vec![Vec::new(); 26];

    let mut dspns: Vec<(u8, Dspn)> = vec![];

    let mut cur_fp = 0;
    let mut cur_cat = 0;

    let mut loaded_image: Option<(usize, image::DynamicImage)> = None;

    let event_loop = EventLoop::new().unwrap();
    let frame = Rc::new(WindowBuilder::new().build(&event_loop).unwrap());

    event_loop.set_control_flow(ControlFlow::Wait);

    let context = softbuffer::Context::new(frame.clone()).unwrap();
    let mut surface = softbuffer::Surface::new(&context, frame.clone()).unwrap();

    let mut no_mods = true;

    event_loop.run(move |event, elwt| match event {
        Event::WindowEvent {
            window_id: _,
            event: WindowEvent::RedrawRequested,
        } => {
            let (width, height) = {
                let size = frame.inner_size();
                (size.width, size.height)
            };

            surface.resize(
                NonZeroU32::new(width).unwrap(),
                NonZeroU32::new(height).unwrap(),
            ).unwrap();

            let mut draw_buffer = surface.buffer_mut().unwrap();

            // println!("State: {:?}", state);

            draw::clear(&mut draw_buffer);

            match state {
                State::Init => {
                    while inp_path.is_none() {
                        draw::ins_text(
                            &mut draw_buffer,
                            (width, height),
                            (0, 0),
                            80,
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
                            _ => match image::io::Reader::open(&file_paths[cur_fp])
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

                        let (oiw, oih) = (orig_image.width(), orig_image.height());

                        if oiw > width || oih > height {
                            orig_image.resize(width, height, image::imageops::Triangle)
                        } else {
                            orig_image.clone()
                        }
                    };

                    let (siw, sih) = (scaled_image.width(), scaled_image.height());
                    let (xofs, yofs) = ((width - siw) / 2, (height - sih) / 2);

                    let img_pix_buf = scaled_image.into_rgb8().into_raw();

                    draw::ins_image(&mut draw_buffer, (width, height), (xofs, yofs), (siw, sih), &img_pix_buf);

                    draw::ins_text(
                        &mut draw_buffer,
                        (width, height),
                        (0, (height - height / 40)),
                        (height / 40) as u8,
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

                    let mut topline = 0;
                    topline += 40 * draw::flow_text(
                        &mut draw_buffer,
                        (width, height),
                        (0, topline),
                        None,
                        40,
                        format!("Dispose Category {}", (cur_cat as u8 + 0x41) as char).as_bytes(),
                    );

                    // display selected image mosaic
                    let (mut imx, mut imy) = (0, topline + 20);
                    let mut crossc = 0;
                    for im in mosaics[cur_cat].iter() {
                        let imb = im.to_rgb8();
                        let imd = imb.dimensions();

                        let (xofs, yofs) = ((MSC_MX_E - imd.0) / 2, (MSC_MX_E - imd.1) / 2);

                        draw::ins_image(&mut draw_buffer, (width, height), (imx + xofs, imy + yofs), imd, &imb);

                        if crossc <= 2 {
                            imx += MSC_MX_E;
                            crossc += 1;
                        } else {
                            imx = 0;
                            imy += MSC_MX_E;
                            crossc = 0;
                        }
                    }

                    topline = imy + MSC_MX_E;

                    // list file names
                    topline += 20;
                    for fp in cats[cur_cat].iter() {
                        draw::ins_text(
                            &mut draw_buffer,
                            (width, height),
                            (0, topline),
                            20,
                            fp.file_name().unwrap().as_encoded_bytes(),
                        );
                        topline += 20;
                    }

                    topline += 20;
                    draw::flow_text(
                        &mut draw_buffer,
                        (width, height),
                        (0, topline),
                        None,
                        30,
                        b"Disposition options: [L]ist files, [C]opy to directory, [Z]ip into archive, [N]othing"
                    );
                }
                State::DspnPath => {
                    while !dspns.is_empty() && out_path.is_none() {
                        draw::ins_text(
                            &mut draw_buffer,
                            (width, height),
                            (0, 0),
                            80,
                            b"Choose output directory...",
                        );

                        out_path = FileDialog::new().set_directory(inp_path.as_ref().unwrap()).pick_folder();
                    }

                    state = State::DspnCom;
                    frame.request_redraw();
                }
                State::DspnCom => {
                    if dspns.is_empty() {
                        elwt.exit();
                        return;
                    }

                    let mut topline = 0;

                    topline += 50 + 50 * draw::flow_text(
                        &mut draw_buffer,
                        (width, height),
                        (0, topline),
                        None,
                        50,
                        format!("Output directory: {:?}", out_path.as_ref().unwrap()).as_bytes(),
                    );

                    for (c, d) in &dspns {
                        let cat = &cats[*c as usize];
                        let cat_char = (c + 0x41) as char;

                        // name
                        draw::ins_text(
                            &mut draw_buffer,
                            (width, height),
                            (0, topline),
                            30,
                            format!("Category {}", cat_char).as_bytes(),
                        );
                        topline += 30;

                        // count
                        draw::ins_text(
                            &mut draw_buffer,
                            (width, height),
                            (0, topline),
                            30,
                            format!("{} images", cat.len()).as_bytes(),
                        );
                        topline += 30;

                        // disp
                        draw::ins_text(
                            &mut draw_buffer,
                            (width, height),
                            (0, topline),
                            30,
                            format!("To {}", match d {
                                Dspn::List => "list",
                                Dspn::Dir => "subfolder",
                                Dspn::Zip => "archive"
                            }).as_bytes(),
                        );
                        topline += 60;
                    }

                    draw::ins_text(
                        &mut draw_buffer,
                        (width, height),
                        (0, topline),
                        40,
                        b"Press <Enter> to commit..."
                    );
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
                Key::Named(NamedKey::Space) => match state {
                    State::Elect => {
                        if cats_assigned[cur_fp].is_some() {
                            cats_assigned[cur_fp] = None;
                            useful_minis[cur_fp] = None;
                        }
                        cur_fp += 1;
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
                                    let name = format!("impack-arc-{cat_char}.zip");
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
        Event::WindowEvent {
            window_id: _,
            event: WindowEvent::CloseRequested,
        } => elwt.exit(),
        _ => (),
    }).unwrap();
}

mod draw {
    const CHARSHEET: &[u8; 60 * 26 * 100 * 3] = include_bytes!("../cs.dat");

    pub fn ins_text(
        buf: &mut dyn std::ops::DerefMut<Target = [u32]>,
        dim: (u32, u32),
        pos: (u32, u32),
        pxh: u8,
        string: &[u8],
    ) {
        let cs = CHARSHEET;

        const P_ROW: u32 = 60 * 26;
        const P_LINE: u32 = P_ROW * 100;

        assert_eq!(cs.len(), P_LINE as usize * 3);

        let (pxh, pxw) = (pxh as u32, (pxh as u32 * 3) / 5);

        for (i, b) in string.iter().enumerate() {
            let ctc = match b {
                b'a'..=b'z' => 60 * (b - b'a') as u32,
                b'A'..=b'Z' => (P_LINE) + (60 * (b - b'A') as u32),
                b','..=b'<' => (P_LINE * 2) + (60 * (b - b',') as u32),
                b'>' | b'?' => (P_LINE * 2) + (60 * (17 + b - b'>') as u32),
                b'!' | b'"' => (P_LINE * 2) + (60 * (19 + b - b'!') as u32),
                b'\'' => (P_LINE * 2) + (60 * (21 + b - b'\'') as u32),
                b'[' => (P_LINE * 2) + (60 * (22 + b - b'[') as u32),
                b']' => (P_LINE * 2) + (60 * (23 + b - b']') as u32),
                b'_' => (P_LINE * 2) + (60 * (24 + b - b'_') as u32),
                b'~' => (P_LINE * 2) + (60 * (25 + b - b'~') as u32),
                b' ' => continue,
                _ => panic!(),
            };

            for cy in 0..100 {
                for cx in 0..60 {
                    let out: u32 = {
                        let ol: u8 = cs[(ctc + (P_ROW * cy) + cx) as usize];
                        // (ol as u32) << 16 + (ol as u32) << 8 + ol as u32
                        (ol as u32) << 16
                    };
                    let (rx, ry) = (cx * pxh / 100, cy * pxh / 100);
                    let head = ((pos.1 + ry) * dim.0) + pos.0 + rx + (i as u32 * pxw);
                    if out != 0 && (head as usize) < buf.len() {
                        buf[head as usize] = out;
                    }
                }
            }
        }
    }

    pub fn flow_text(
        buf: &mut dyn std::ops::DerefMut<Target = [u32]>,
        dim: (u32, u32),
        pos: (u32, u32),
        maxw: Option<u32>,
        pxh: u8,
        string: &[u8],
    ) -> u32 {
        let pxw = (pxh as u32 * 3) / 5;

        let avail = dim.0 - pos.0;
        let maxw = match maxw {
            Some(m) => m.min(avail),
            None => avail,
        };

        let maxc = maxw / pxw;
        let lines = 1 + (string.len() / maxc as usize);

        let mut ll = lines;
        let mut curh = pos.1;
        let mut stac: usize = 0;

        loop {
            let lasc = stac + (maxc as usize).min(string.len() - stac);
            ins_text(buf, dim, (pos.0, curh), pxh, &string[stac..lasc]);

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

                buf[op as usize] = pxl;
            }
        }
    }

    pub fn clear(buf: &mut dyn std::ops::DerefMut<Target = [u32]>) {
        for p in 0..buf.len() {
            buf[p] = (255 << 16) + (255 << 8) + 255
        }
    }
}
