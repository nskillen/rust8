extern crate sdl2;

use sdl2::keyboard::Keycode;

pub const NUM_KEYS : usize = 16;

#[derive(Debug, Default)]
pub struct Kb {
	key : [KeyState; NUM_KEYS],
    last_pressed: u8,
    last_released: u8,
}

impl Kb {
	pub fn key_state(&self, key: usize) -> KeyState {
		self.key[key]
	}

    pub fn press(&mut self, key: sdl2::keyboard::Keycode) {
        match get_key_idx(key) {
            Some(key_idx) => {
                self.key[key_idx] = KeyState::Down;
                self.last_pressed = key_idx as u8;
            },
            None => return,
        }
    }

    pub fn release(&mut self, key: sdl2::keyboard::Keycode) {
        match get_key_idx(key) {
            Some(key_idx) => {
                self.key[key_idx] = KeyState::Up;
                self.last_released = key_idx as u8;
            },
            None => return,
        }
    }

    pub fn last_pressed(&self) -> u8 { self.last_pressed }
    pub fn last_released(&self) -> u8 { self.last_released }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum KeyState {
	Up,
	Down,
}

impl Default for KeyState {
	fn default() -> Self {
		KeyState::Up
	}
}


fn get_key_idx(key: sdl2::keyboard::Keycode) -> Option<usize> {
    match key {
        Keycode::Num0     => Some(0x00),
        Keycode::Num1     => Some(0x01),
        Keycode::Num2     => Some(0x02),
        Keycode::Num3     => Some(0x03),
        Keycode::Num4     => Some(0x04),
        Keycode::Num5     => Some(0x05),
        Keycode::Num6     => Some(0x06),
        Keycode::Num7     => Some(0x07),
        Keycode::Num8     => Some(0x08),
        Keycode::Num9     => Some(0x09),

        Keycode::Kp0      => Some(0x00),
        Keycode::Kp1      => Some(0x01),
        Keycode::Kp2      => Some(0x02),
        Keycode::Kp3      => Some(0x03),
        Keycode::Kp4      => Some(0x04),
        Keycode::Kp5      => Some(0x05),
        Keycode::Kp6      => Some(0x06),
        Keycode::Kp7      => Some(0x07),
        Keycode::Kp8      => Some(0x08),
        Keycode::Kp9      => Some(0x09),

        Keycode::Insert   => Some(0x0A),
        Keycode::Home     => Some(0x0B),
        Keycode::PageUp   => Some(0x0C),
        Keycode::Delete   => Some(0x0D),
        Keycode::End      => Some(0x0E),
        Keycode::PageDown => Some(0x0F),

        _ => None,
    }
}
