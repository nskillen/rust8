pub const NUM_KEYS : usize = 16;

#[derive(Debug, Default)]
pub struct Kb {
    key : [KeyState; NUM_KEYS],
}

impl Kb {
    pub fn key_state(&self, key: usize) -> KeyState {
        self.key[key]
    }

    pub fn get_next_keypress_blocking(&mut self) -> u8 {
        panic!("Don't know how to wait for a keypress yet");
    }
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
