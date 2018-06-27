pub mod cpu;

pub trait Memory {
    fn get(&self, address: u16) -> u8;
    fn set(&self, address: u16, data: u8);
}
