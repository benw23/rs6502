use crate::hdw::Memory;
use std::cell::Cell;

pub struct Mem {
    pub data: Vec<Cell<u8>>,
}

impl Mem {
    pub fn load(&self, data: Vec<u8>, location: u16, reset: u16) {
        for (index, value) in data.iter().enumerate() {
            self.set((index as u16).wrapping_add(location), *value)
        }
        self.set(0xFFFD, (reset >> 8) as u8);
        self.set(0xFFFC, (reset % 256) as u8);
    }

    pub fn load_file(&self, data: &str, location: u16, reset: u16) {
        use std::fs::File;
        use std::io::Read;

        let mut file = File::open(data).unwrap();
        let mut buf = vec![];

        file.read_to_end(&mut buf).unwrap();

        buf.truncate(0x4000);
        self.load(buf, location, reset);
    }
}

impl Memory for Mem {
    fn get(&self, address: u16) -> u8 {
        self.data[address as usize].get()
    }
    fn set(&self, address: u16, data: u8) {
        self.data[address as usize].set(data);
    }
}
