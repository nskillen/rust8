use super::hw;

const NUM_GP_REGISTERS : usize = 16; // number of general-purpose registers
const TIMER_DECAY_FREQ : usize = 60;  // Hz
pub const STACK_SIZE   : usize = 0x0010; // 16 stack addresses

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

    delta_ns_total: u64,
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

            delta_ns_total: 0,
		}
	}

    pub fn decay_timers(&mut self, delta_ns: u64) {
        self.delta_ns_total += delta_ns;
        if self.delta_ns_total >= 1e9 as u64 / TIMER_DECAY_FREQ as u64 {

            if self.reg_delay > 0 { self.reg_delay -= 1; }
            if self.reg_sound > 0 { self.reg_sound -= 1; }
            
            self.delta_ns_total -= 1e9 as u64 / TIMER_DECAY_FREQ as u64;
        }
    }
}
