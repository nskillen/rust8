extern crate rand;
extern crate tcod;

use std::env;
use std::fs;
use std::io::Read;
use std::path::Path;

use tcod::console::Root;

mod hw;
mod cpu;
mod kb;

fn main() {
    let mut root_initializer = Root::initializer();

    let program_file = env::args().nth(1).unwrap();
    let font_file = env::args().nth(2).unwrap();
    
    let program_code = read_bin(program_file);

    root_initializer.font(font_file, tcod::console::FontLayout::AsciiInCol);

    let mut hw = hw::Hw::new(&mut root_initializer);

    // Load program code in memory
    hw.load_memory(hw::INTERP_LENGTH, &program_code[..]);
    hw.run();

    println!("{:#?}", hw);
}

fn read_bin<P: AsRef<Path>>(path: P) -> Vec<u8> {
    let mut file = fs::File::open(path).unwrap();
    let mut file_buf = Vec::new();
    file.read_to_end(&mut file_buf).unwrap();
    file_buf
}
