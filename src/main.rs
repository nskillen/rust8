extern crate core;
extern crate rand;
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
	let program_code = match read_bin(program_file) {
        Some(code) => code,
        None => {
            println!("Unable to read program code");
            return;
        },
    };

	let mut hw = match hw::Hw::new() {
        Some(hw) => hw,
        None => {
            println!("Unable to initialize hardware!");
            return;
        }
    };

	// Load program code in memory
	hw.load_memory(hw::INTERP_LENGTH, &program_code[..]);
	hw.run();

	println!("{:#?}", hw);
}

fn read_bin<P: AsRef<Path>>(path: P) -> Option<Vec<u8>> {
	let mut file = match fs::File::open(path) {
        Ok(f) => f,
        Err(err) => {
            println!("Error opening file: {}", err);
            return None;
        },
    };

	let mut file_buf = Vec::new();
	match file.read_to_end(&mut file_buf) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return None;
        },
    };

	Some(file_buf)
}
