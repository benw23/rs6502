#![allow(dead_code)]
mod hdw;
mod mem;
use std::cell::Cell;

fn main() {

    let memory = mem::Mem {
        data: vec![Cell::new(0); 65536],
    };

    memory.load_file("nestest.nes", 0xbff0, 0x4000);
    memory.load_file("nestest.nes", 0x7ff0, 0x4000);
    // memory.load(vec![0x4a, 0], 0xc000, 0xc000);

    let mut cpu = hdw::cpu::Cpu::new(&memory, 0);

    cpu.pc = 0xc000;
    cpu.s = 0xfd;

    for _i in 0..10000 {
        println!(
            "{:04X} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            cpu.pc, cpu.a, cpu.x, cpu.y, cpu.flags, cpu.s
        );

        cpu.tick();
    }
}
