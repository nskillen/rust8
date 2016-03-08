extern crate rand;
//extern crate glium_sdl2;
extern crate sdl2;

use rand::Rng;
//use glium_sdl2::{DisplayBuild,SDL2Facade};
use sdl2::EventPump;
use sdl2::render::Renderer;

use time;

use std::fmt;
use std::iter::repeat;

use super::cpu;
use super::kb;

const MEMORY_SIZE       : usize = 0x1000; // 4096 bytes of RAM

const INTERP_START      : usize = 0x0000; // interpreter start address
const INTERP_END        : usize = 0x0200; // interpreter end address
pub const INTERP_LENGTH : usize = INTERP_END - INTERP_START;

const HEX_SPRITES_START : usize = 0x0000; // put hex sprites right at the start
const HEX_SPRITE_SIZE   : usize = 5;	  // each sprite is 5 bytes
const HEX_SPRITE_COUNT  : usize = 16;	 // there are 16 of them, 0-9A-F

const INSTRUCTION_SIZE  : usize = 2;

const DELAY_RATE_HZ	    : usize = 60; // countdown frequency of the delay register
const SOUND_RATE_HZ	    : usize = 60; // countdown frequency of the sound register

const DISPLAY_WIDTH	    : usize = 64; // pixels per row
const DISPLAY_HEIGHT    : usize = 32; // number of rows

const PIXEL_WIDTH       : usize = 10; // how many pixels wide
const PIXEL_HEIGHT      : usize = 10; // and high each CHIP-8 pixel is on screen

const DEBUG_MEMORY_PRINT_WIDTH : usize = 64;

const CPU_SPEED         : usize = 512; // Hz

pub struct Hw<'a> {
	cpu: cpu::Cpu,

	// 4Kb of memory
	memory: [u8; MEMORY_SIZE],
	display_memory: [u8; DISPLAY_WIDTH * DISPLAY_HEIGHT / 8], // size in bytes, thanks to conversion above
	rng: rand::ThreadRng,

	kb: kb::Kb,

	display: sdl2::render::Renderer<'a>,
	event_pump: sdl2::EventPump,

    waiting_for_keypress: bool,
    keypress_store_to: usize,

    quit_requested: bool,

    last_instruction_time: u64,
}

impl<'a> Hw<'a>  {
	pub fn new() -> Option<Hw<'a>> {
		let sdl_context = match sdl2::init() {
            Ok(context) => context,
            Err(err) => { println!("Error initializing SDL2: {}", err); return None },
        };

		let video_subsystem = match sdl_context.video() {
            Ok(video) => video,
            Err(err) => { println!("Error starting video subsystem: {}", err); return None },
        };

		let window = match video_subsystem.window("CHIP-8", (PIXEL_WIDTH * DISPLAY_WIDTH) as u32, (PIXEL_HEIGHT * DISPLAY_HEIGHT) as u32)
                                .position_centered().opengl().build() {
            Ok(display) => display,
            Err(err) => { println!("Error creating window: {}", err); return None },
        };

        let renderer = match window.renderer().build() {
            Ok(renderer) => renderer,
            Err(err) => { println!("Unable to create renderer: {}", err); return None },
        };

		let mut event_pump = match sdl_context.event_pump() {
            Ok(pump) => pump,
            Err(err) => { println!("Error creating event pump: {}", err); return None },
        };

		Some(Hw {
			cpu: cpu::Cpu::new(INTERP_END as u16),
			memory: [0; MEMORY_SIZE],
			display_memory: [0; DISPLAY_WIDTH * DISPLAY_HEIGHT / 8],
			rng: rand::thread_rng(),
			kb: kb::Kb::default(),

			display: renderer,
			event_pump: event_pump,

            waiting_for_keypress: false,
            keypress_store_to: 0,

            quit_requested: false,

            last_instruction_time: 0,
		})
	}

	pub fn load_memory(&mut self, base: usize, data: &[u8]) {
		for offset in 0..data.len() {
			self.memory[base + offset] = data[offset];
		}
	}

	pub fn run(&mut self) {
        self.last_instruction_time = time::precise_time_ns();

		while !self.quit_requested {
            if !self.waiting_for_keypress {
    			self.execute_next_instruction();
            }
            self.decay_timers();
            self.handle_events();
            self.draw();
		}
	}

    fn decay_timers(&mut self) {
        let delta_ns = time::precise_time_ns() - self.last_instruction_time;
        self.cpu.decay_timers(delta_ns);
        self.last_instruction_time += delta_ns;
    }

    fn handle_events(&mut self) {
        for event in self.event_pump.poll_iter() {
            use sdl2::event::Event;
            match event {
                Event::Quit { .. } => {
                    self.quit_requested = true
                },
                Event::KeyDown { timestamp, window_id, keycode, scancode, keymod, repeat } => {
                    if repeat { continue; }
                    match keycode {
                        None => continue,
                        Some(kc) => {
                            println!("Key pressed: {}", kc);
                            self.kb.press(kc);
                            if self.waiting_for_keypress {
                                self.waiting_for_keypress = false;
                                self.cpu.v[self.keypress_store_to] = self.kb.last_pressed();
                            }
                        },
                    }
                },
                Event::KeyUp { timestamp, window_id, keycode, scancode, keymod, repeat } => {
                    if repeat { continue; }
                    match keycode {
                        None => continue,
                        Some(kc) => self.kb.release(kc),
                    }
                },
                _ => ()
            }
        }
    }

    fn draw(&mut self) {
        use sdl2::pixels::Color;
        let black = sdl2::pixels::Color::RGB(0x00, 0x00, 0x00);
        let white = sdl2::pixels::Color::RGB(0xFF, 0xFF, 0xFF);

        let _ = self.display.set_draw_color(black);
        let _ = self.display.clear();

        let _ = self.display.set_draw_color(white);

        for row in 0..DISPLAY_HEIGHT {
            let base = row * (DISPLAY_WIDTH / 8);

            for byte in 0..(DISPLAY_WIDTH / 8) {
                if self.display_memory[base + byte] == 0 { continue };

                for bit in (0..8).rev() {
                    use sdl2::rect::Rect;
                    if self.display_memory[base + byte] & (1 << bit) == 0 { continue; }
                    let x = ((8 * byte + (7-bit)) * PIXEL_WIDTH) as i32;
                    let y = (row * PIXEL_HEIGHT) as i32;
                    let pixel = Rect::new(x, y, PIXEL_WIDTH as u32, PIXEL_HEIGHT as u32).unwrap().unwrap();
                    let _ = self.display.fill_rect(pixel);
                }
            }
        }

        let _ = self.display.present();
    }

	pub fn execute_next_instruction(&mut self) {
		let instruction : u16 = (self.memory[self.cpu.pc as usize] as u16) << 8 | (self.memory[(self.cpu.pc + 1) as usize] as u16);

		self.cpu.pc += INSTRUCTION_SIZE as u16; // increment program counter by 2 bytes. Done now, so that we don't increment after jumping

		// these are some commonly used values
		let dest_addr = instruction & 0x0FFF;
		let left_reg = (instruction >> 8) & 0x0F;
		let right_reg = (instruction >> 4) & 0x0F;
		let byte = instruction as u8;

		match instruction & 0xF000 {
			0x0000 => {
				match instruction & 0x00FF {
					0x00E0 => {
						// CLS
						self.display_memory = [0; DISPLAY_WIDTH * DISPLAY_HEIGHT / 8];
					},
					0x00EE => {
						// RET
						// set pc to address on top of stack, then decrement stack
						// pointer
						match self.cpu.sp {
							0 => panic!("Tried to return when not in a function!"),
							_ => {
								self.cpu.sp -= 1;
								self.cpu.pc = self.cpu.stack[self.cpu.sp as usize];
							}
						}
					},
					_ => panic!("Unknown 0x0000-series instruction: {:#x}", instruction)
				}
			},
			0x1000 => {
				// JMP
				self.cpu.pc = dest_addr;
			},
			0x2000 => {
				// CALL
				if self.cpu.sp == cpu::STACK_SIZE as u8 {
					panic!("Tried to call into a function, but stack was full!");
				}
				self.cpu.stack[self.cpu.sp as usize] = self.cpu.pc;
				self.cpu.sp += 1;
				self.cpu.pc = dest_addr;
			},
			0x3000 => {
				// SE Vx, byte
				// skip next instruction if Vx == byte
				if self.cpu.v[left_reg as usize] == byte {
					self.cpu.pc += INSTRUCTION_SIZE as u16;
				}
			},
			0x4000 => {
				// SNE Vx, byte
				// skip next instruction if Vx != byte
				if self.cpu.v[left_reg as usize] != byte {
					self.cpu.pc += INSTRUCTION_SIZE as u16;
				}
			},
			0x5000 => {
				// SE Vx, Vy
				// skip next instruction if Vx == Vy
				if self.cpu.v[left_reg as usize] == self.cpu.v[right_reg as usize] {
					self.cpu.pc += INSTRUCTION_SIZE as u16;
				}
			},
			0x6000 => {
				// LD Vx, byte
				// put the value 'byte' into Vx
				self.cpu.v[left_reg as usize] = byte;
			},
			0x7000 => {
				// ADD Vx, byte
				self.cpu.v[left_reg as usize] = (self.cpu.v[left_reg as usize] as u16 + byte as u16) as u8;
			},
			0x8000 => {
				match instruction & 0xF00F {
					0x8000 => {
						// LD Vx, Vy
						self.cpu.v[left_reg as usize] = self.cpu.v[right_reg as usize];
					},
					0x8001 => {
						// OR Vx, Vy
						self.cpu.v[left_reg as usize] |= self.cpu.v[right_reg as usize];
					},
					0x8002 => {
						// AND Vx, Vy
						self.cpu.v[left_reg as usize] &= self.cpu.v[right_reg as usize];
					},
					0x8003 => {
						// XOR Vx, Vy
						self.cpu.v[left_reg as usize] ^= self.cpu.v[right_reg as usize];
					},
					0x8004 => {
						// ADD Vx, Vy
						let a = self.cpu.v[left_reg as usize] as u16;
						let b = self.cpu.v[right_reg as usize] as u16;
						let c = a + b;
						self.cpu.v[0x0F] = if c > 0xFF { 1 } else { 0 };
						self.cpu.v[left_reg as usize] = c as u8;
					},
					0x8005 => {
						// SUB Vx, Vy
						let a = self.cpu.v[left_reg as usize] as i16;
						let b = self.cpu.v[right_reg as usize] as i16;
						let c = a - b;
						self.cpu.v[0x0F] = if a > b { 1 } else { 0 };
						self.cpu.v[left_reg as usize] = c as u8;
					},
					0x8006 => {
						// SHR Vx{, Vy}
						self.cpu.v[0x0F] = self.cpu.v[left_reg as usize] % 2 as u8;
						self.cpu.v[left_reg as usize] >>= 1;
					},
					0x8007 => {
						// SUBN Vx, Vy
						let b = self.cpu.v[left_reg as usize] as u16;
						let a = self.cpu.v[right_reg as usize] as u16;
						let c = a - b;
						self.cpu.v[0x0F] = if a > b { 1 } else { 0 };
						self.cpu.v[left_reg as usize] = c as u8;
					},
					0x800E => {
						// SHL Vx{, Vy}
						self.cpu.v[0x0F] = if self.cpu.v[left_reg as usize] & 0x80 != 0 { 1 } else { 0 };
						self.cpu.v[left_reg as usize] <<= 1;
					},
					_ => panic!("Unknown 0x8000-series instruction: {:#x}", instruction)
				}
			},
			0x9000 => {
				match instruction & 0xF00F {
					0x9000 => {
						// SNE Vx, Vy
						if self.cpu.v[left_reg as usize] != self.cpu.v[right_reg as usize] {
							self.cpu.pc += INSTRUCTION_SIZE as u16;
						}
					},
					_ => panic!("Unknown 0x9000-series instruction: {:#x}", instruction)
				}
			},
			0xA000 => {
				// LD I, addr
				self.cpu.i = dest_addr;
			},
			0xB000 => {
				// JMP V0, addr
				self.cpu.pc = (self.cpu.v[0x00] as u16) + dest_addr;
			},
			0xC000 => {
				// RND Vx, byte
				// the interpreter generates a random value 0-255, which is ANDed with
				// byte, and stored in Vx
				let random_value = self.rng.gen::<u8>();
				let result = random_value & byte;
				self.cpu.v[left_reg as usize] = result;
			},
			0xD000 => {
				// DRW Vx, Vy, nibble
				//
				// draws an n-byte sprite onto screen at location (Vx, Vy) via XOR into
				// display memory
				//
				// sprite bytes are taken from memory starting at address in register I
				//
				// If any pixels are turned off, then set VF to 1, else set it to 0
				//
				// Overdraws wrap around

				let num_bytes = instruction & 0x0F;
				let sprite_bytes = &mut self.memory[(self.cpu.i as usize)..((self.cpu.i+num_bytes) as usize)];
				let x = self.cpu.v[left_reg as usize];
				let mut y = self.cpu.v[right_reg as usize];

				let mut collision = false;
				for byte in sprite_bytes {
					if y >= DISPLAY_HEIGHT as u8 {
						y -= DISPLAY_HEIGHT as u8;
					}

					if x % 8 == 0 {
						// sprite is entirely contained within a display byte,
						// so no need for any fancy crap to split the bits

						let offset = y as usize * (DISPLAY_WIDTH / 8) + (x / 8) as usize;
						let old_byte = self.display_memory[offset];
						let new_byte = *byte ^ old_byte;

						collision |= old_byte & *byte != 0;
						self.display_memory[offset as usize] = new_byte;
					} else {
						let split = x % 8; // where in the byte the sprite is split

						let left_off = y * (DISPLAY_WIDTH / 8) as u8 + (x / 8);
						let left_b_old_val = self.display_memory[left_off as usize];
						let left_b_new_val = *byte >> split;

						collision |= left_b_old_val & left_b_new_val != 0;
						self.display_memory[left_off as usize] ^= left_b_new_val;


						let mut right_off = if left_off == 0xFF { 0x00 } else { left_off + 1 };

						let right_b_old_val = self.display_memory[right_off as usize];
						let right_b_new_val = (*byte & ((1 << split) - 1)) << (8-split);

						collision |= right_b_old_val & right_b_new_val != 0;
						self.display_memory[right_off as usize] ^= right_b_new_val;
					}
					y += 1;
				}
				self.cpu.v[0x0F] = if collision { 1 } else { 0 };
			},
			0xE000 => {
				match instruction & 0xF0FF {
					0xE09E => {
						// SKP Vx
						// skip next instruction if the key with the value of Vx is
						// pressed
						let key = self.cpu.v[((instruction >> 8) & 0x0F) as usize] as usize;
						if self.kb.key_state(key) == kb::KeyState::Down {
							self.cpu.pc += INSTRUCTION_SIZE as u16;
						}
					},
					0xE0A1 => {
						// SKNP Vx
						// skip next instruction if the key with the value of Vx is not
						// pressed
						let key = self.cpu.v[((instruction >> 8) & 0x0F) as usize] as usize;
						if self.kb.key_state(key) == kb::KeyState::Up {
							self.cpu.pc += INSTRUCTION_SIZE as u16;
						}
					},
					_ => panic!("Unknown 0xE000-series instruction: {:#x}", instruction)
				}
			},
			0xF000 => {
				let reg = (instruction >> 8) & 0x0F;
				match instruction & 0xF0FF {
					0xF007 => {
						// LD Vx, DT
						// set Vx to delay timer value
						self.cpu.v[reg as usize] = self.cpu.reg_delay;
					},
					0xF00A => {
						// LD Vx, K
						// wait for keypress, and then put value of key pressed into Vx
                        self.keypress_store_to = reg as usize;
                        self.waiting_for_keypress = true;
					},
					0xF015 => {
						// LD DT, Vx
						// set delay timer with value in Vx
						self.cpu.reg_delay = self.cpu.v[reg as usize];
					},
					0xF018 => {
						// LD ST, Vx
						// set sound timer with value in Vx
						self.cpu.reg_sound = self.cpu.v[reg as usize];
					},
					0xF01E => {
						// ADD I, Vx
						// I = I + Vx
						self.cpu.i += self.cpu.v[reg as usize] as u16;
					},
					0xF029 => {
						// LD F, Vx
						// set I to location of Hex sprite corresponding to value of Vx
						let sprite = self.cpu.v[reg as usize] as u16;
						if sprite > 0x0F {
							panic!("Requested hex sprite '{:#x}' is not valid", sprite);
						}
						self.cpu.i = (HEX_SPRITES_START + (HEX_SPRITE_SIZE * sprite as usize)) as u16;
					},
					0xF033 => {
						// LD B, Vx
						// store BCD representation of of Vx in memory locations I,
						// I+1, and I+2
						let val = self.cpu.v[reg as usize];
						let offset = self.cpu.i as usize;
						self.memory[offset + 0] = (val / 100) as u8;
						self.memory[offset + 1] = ((val / 10) % 10) as u8;
						self.memory[offset + 2] = (val % 10) as u8;
					},
					0xF055 => {
						// LD [I], Vx
						// store registers V0 through Vx in memory, starting at [I]
						for reg_off in 0..(reg as usize)+1 {
							self.memory[self.cpu.i as usize + reg_off] = self.cpu.v[reg_off]
						}
					},
					0xF065 => {
						// LD Vx, [I]
						// read memory starting at [I] into registers V0..Vx
						for reg_off in 0..(reg as usize)+1 {
							self.cpu.v[reg_off] = self.memory[self.cpu.i as usize + reg_off]
						}
					},
					_ => panic!("Unknown 0xF000-series instruction: {:#x}", instruction)
				}
			},
			_ => panic!("Uknown instruction: {:#x}", instruction)
		}
	}
	
	fn fmt_memory(&self, f: &mut fmt::Formatter) -> fmt::Result {
		writeln!(f, "\tmemory: [");
		write!(f, "\t\t      \t");
		for group in 0..(DEBUG_MEMORY_PRINT_WIDTH / 8) {
			write!(f, "0x{:02x}                     ", (0x08 * group));
		}
		writeln!(f, "");
		for row in 0..(MEMORY_SIZE / DEBUG_MEMORY_PRINT_WIDTH) {
			write!(f, "\t\t");
			write!(f, "0x{:04x}\t", row * DEBUG_MEMORY_PRINT_WIDTH);
			for offset in 0..(DEBUG_MEMORY_PRINT_WIDTH / 8) {
				write!(f, "{:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}  ",
					   self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x00], self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x01],
					   self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x02], self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x03],
					   self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x04], self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x05],
					   self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x06], self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x07]);
			}
			write!(f, "\n");
		}
		write!(f, "\t\t      \t");
		for group in 0..(DEBUG_MEMORY_PRINT_WIDTH / 8) {
			write!(f, "                   0x{:02x}  ", (0x08 * (group+1) - 1));
		}
		writeln!(f, "");
		writeln!(f, "\t],")
	}

	fn fmt_display_memory(&self, f: &mut fmt::Formatter) -> fmt::Result {
		writeln!(f, "\t display_memory: [");
		writeln!(f, "\t\t  {}", repeat('-').take(DISPLAY_WIDTH).collect::<String>());
		for row in 0..DISPLAY_HEIGHT {
			write!(f, "\t\t| ");
			for byte in 0..(DISPLAY_WIDTH / 8) {
				for bit in (0..8).rev() {
					write!(f, "{}", if self.display_memory[row * (DISPLAY_WIDTH / 8) + byte] & (1 << bit) != 0 { "#" } else { "." });
				}
			}
			writeln!(f, " |");
		}
		writeln!(f, "\t\t  {}", repeat('-').take(DISPLAY_WIDTH).collect::<String>());
		writeln!(f, "\t],")
	}
}

impl<'a> fmt::Debug for Hw<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		writeln!(f, "Hw: {{");
		writeln!(f, "\tcpu: {:#?}", self.cpu);
		self.fmt_memory(f);
		self.fmt_display_memory(f);
        writeln!(f, "\twaiting_for_keypress: {:#?}", self.waiting_for_keypress);
        writeln!(f, "\tkeypress_store_to: {:#02X}", self.keypress_store_to);
		writeln!(f, "}}")
	}
}
