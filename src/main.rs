extern crate rand;

#[macro_use]
extern crate glium;
extern crate glium_sdl2;
extern crate sdl2;

use std::env;
use std::fs;
use std::io::Read;
use std::path::Path;


mod hw;
mod cpu;
mod kb;

fn main() {
	let program_file = env::args().nth(1).unwrap();
	let program_code = read_bin(program_file);

	let mut hw = hw::Hw::new();

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
