use super::hw;

use std::time::{Duration};

const NUM_GP_REGISTERS : usize = 16; // number of general-purpose registers
pub const STACK_SIZE   : usize = 0x0010; // 16 stack addresses
pub const FREQUENCY    : u32   = 512; // MHz

const DELAY_RATE_HZ	    : u32 = 60; // countdown frequency of the delay register
const SOUND_RATE_HZ	    : u32 = 60; // countdown frequency of the sound register

pub const DELAY_TIMER_DECAY_TICK : u32 = 1_000_000_000 / DELAY_RATE_HZ; // 60 Hz
pub const SOUND_TIMER_DECAY_TICK : u32 = 1_000_000_000 / SOUND_RATE_HZ; // 60 Hz


#[derive(Debug)]
pub struct Cpu {
	// 16 8-bit general purpose registers
	//
	// v[0x0F] should never be used to store data, since some
	// instructions use it as a flag register
	pub v: [u8; NUM_GP_REGISTERS],

	// 1 16-bit register (usually holds addresses, though not required)
	pub i: u16,


	pub reg_delay: u8, // delay register
	pub reg_sound: u8, // sound register

	pub pc: u16, // program counter
	pub sp: u8,  // stack pointer
	pub stack: [u16; STACK_SIZE],

    delay_delta: Duration,
    sound_delta: Duration,
}

impl Cpu {
	pub fn new(initial_pc: u16) -> Cpu {
		Cpu {
			v: [0u8; NUM_GP_REGISTERS],
			i: 0u16,
			reg_delay: 0u8,
			reg_sound: 0u8,
			pc: initial_pc,
			sp: 0u8,
			stack: [0u16; STACK_SIZE],

            delay_delta: Duration::new(0,0),
            sound_delta: Duration::new(0,0),
		}
	}

    pub fn decay_timers(&mut self, time_taken: Duration) {
        use core::ops::{Add,Sub};
        
        let delay_tick = Duration::new(0,DELAY_TIMER_DECAY_TICK);
        self.delay_delta = self.delay_delta.add(time_taken);
        if self.delay_delta.gt(&delay_tick) {
            if self.reg_delay > 0 { self.reg_delay -= 1; }
            self.delay_delta = self.delay_delta.sub(delay_tick);
        }

        let sound_tick = Duration::new(0,SOUND_TIMER_DECAY_TICK);
        self.sound_delta = self.sound_delta.add(time_taken);
        if self.sound_delta.gt(&sound_tick) {
            if self.reg_sound > 0 { self.reg_sound -= 1; }
            self.sound_delta = self.sound_delta.sub(sound_tick);
        }
    }
}

