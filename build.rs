// Impack, a tool for interactively sorting images.

// Copyright © 2023 Matthew Rothlisberger
// SPDX-License-Identifier: GPL-3.0-only

// Impack is licensed under the terms of the GNU General Public
// License, version 3 only. See the top level LICENSES directory for
// the license text.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// build.rs

// <>

fn main() {
    println!("cargo:rerun-if-changed=charsheet.png");

    let charsheet: Vec<u8> = {
        let f = image::io::Reader::open("charsheet.png").unwrap();
        f.decode().unwrap().into_luma8().into_vec()
    };

    std::fs::write("cs.dat", charsheet).unwrap();
}
