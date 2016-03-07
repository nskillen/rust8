extern crate rand;
extern crate tcod;

use rand::Rng;
use std::fmt;
use std::iter::repeat;

use tcod::console::Root;

use super::cpu;
use super::kb;

const MEMORY_SIZE       : usize = 0x1000; // 4096 bytes of RAM

const INTERP_START      : usize = 0x0000; // interpreter start address
const INTERP_END        : usize = 0x0200; // interpreter end address
pub const INTERP_LENGTH : usize = INTERP_END - INTERP_START;

const HEX_SPRITES_START : usize = 0x0000; // put hex sprites right at the start
const HEX_SPRITE_SIZE   : usize = 5;      // each sprite is 5 bytes
const HEX_SPRITE_COUNT  : usize = 16;     // there are 16 of them, 0-9A-F

const INSTRUCTION_SIZE  : usize = 2;

const DELAY_RATE_HZ     : usize = 60; // countdown frequency of the delay register
const SOUND_RATE_HZ     : usize = 60; // countdown frequency of the sound register

const DISPLAY_WIDTH     : usize = 64 / 8; // pixels per row over pixels per byte == bytes per row
const DISPLAY_HEIGHT    : usize = 32;     // number of rows

const DEBUG_MEMORY_PRINT_WIDTH : usize = 64;

pub struct Hw {
    cpu: cpu::Cpu,

    // 4Kb of memory
    memory: [u8; MEMORY_SIZE],
    display: [u8; DISPLAY_WIDTH * DISPLAY_HEIGHT], // size in bytes, thanks to conversion above
    rng: rand::ThreadRng,

    kb: kb::Kb,

    console: tcod::console::Root,
}

impl Hw {
    pub fn new(console_initializer: &mut tcod::console::RootInitializer) -> Hw {
        let root_console = console_initializer
            .size(DISPLAY_WIDTH as i32, DISPLAY_HEIGHT as i32)
            .title("CHIP-8")
            .fullscreen(false)
            .init();

        Hw {
            cpu: cpu::Cpu::new(INTERP_END as u16),
            memory: [0; MEMORY_SIZE],
            display: [0; DISPLAY_WIDTH * DISPLAY_HEIGHT],
            rng: rand::thread_rng(),
            kb: kb::Kb::default(),
            console: root_console,
        }
    }

    pub fn load_memory(&mut self, base: usize, data: &[u8]) {
        for offset in 0..data.len() {
            self.memory[base + offset] = data[offset];
        }
    }

    pub fn run(&mut self) {
        loop {
            self.execute_next_instruction();
        }
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
                        self.display = [0; DISPLAY_WIDTH * DISPLAY_HEIGHT];
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
                self.cpu.v[left_reg as usize] += byte;
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
                        let a = self.cpu.v[left_reg as usize] as u16;
                        let b = self.cpu.v[right_reg as usize] as u16;
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

                        let offset = y * DISPLAY_WIDTH as u8 + x;
                        let old_byte = self.display[offset as usize];
                        let new_byte = *byte ^ old_byte;

                        collision |= old_byte & *byte != 0;
                        self.display[offset as usize] = new_byte;
                    } else {
                        let split = x % 8; // where in the byte the sprite is split

                        let left_off = y * DISPLAY_WIDTH as u8 + (x / 8);
                        let left_b_old_val = self.display[left_off as usize];
                        let left_b_new_val = *byte >> split;

                        collision |= left_b_old_val & left_b_new_val != 0;
                        self.display[left_off as usize] ^= left_b_new_val;


                        let mut right_off = if left_off == 0xFF { 0x00 } else { left_off + 1 };

                        let right_b_old_val = self.display[right_off as usize];
                        let right_b_new_val = (*byte & ((1 << split) - 1)) << (8-split);

                        collision |= right_b_old_val & right_b_new_val != 0;
                        self.display[right_off as usize] ^= right_b_new_val;
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
                        self.cpu.v[reg as usize] = self.kb.get_next_keypress_blocking();
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
            write!(f, "0x{:02x}                       ", (0x08 * group));
        }
        writeln!(f, "");
        for row in 0..(MEMORY_SIZE / DEBUG_MEMORY_PRINT_WIDTH) {
            write!(f, "\t\t");
            write!(f, "0x{:04x}\t", row * DEBUG_MEMORY_PRINT_WIDTH);
            for offset in 0..(DEBUG_MEMORY_PRINT_WIDTH / 8) {
                write!(f, "{:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}    ",
                       self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x00], self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x01],
                       self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x02], self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x03],
                       self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x04], self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x05],
                       self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x06], self.memory[row * DEBUG_MEMORY_PRINT_WIDTH + offset + 0x07]);
            }
            write!(f, "\n");
        }
        write!(f, "\t\t      \t");
        for group in 0..(DEBUG_MEMORY_PRINT_WIDTH / 8) {
            write!(f, "                   0x{:02x}    ", (0x08 * (group+1) - 1));
        }
        writeln!(f, "");
        writeln!(f, "\t],")
    }

    fn fmt_display(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "\t display: [");
        writeln!(f, "\t\t  {}", repeat('-').take(DISPLAY_WIDTH).collect::<String>());
        for row in 0..DISPLAY_HEIGHT {
            writeln!(f, "\t\t| {} |", repeat('.').take(DISPLAY_WIDTH).collect::<String>());
        }
        writeln!(f, "\t\t  {}", repeat('-').take(DISPLAY_WIDTH).collect::<String>());
        writeln!(f, "\t],")
    }
}

impl fmt::Debug for Hw {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Hw: {{");
        writeln!(f, "\tcpu: {:#?}", self.cpu);
        self.fmt_memory(f);
        self.fmt_display(f);
        writeln!(f, "}}")
    }
}
