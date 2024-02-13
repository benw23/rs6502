use crate::hdw::Memory;

use self::ops::dcp;
mod ops;

pub struct Cpu<'a> {
    pub pc: u16,
    pub s: u8,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub flags: u8,
    pub mem: &'a (dyn Memory + 'a),
    cycles: u8,
    irq: bool,
    nmi: bool,
    reset: bool,
}

impl<'a> Cpu<'a> {
    pub fn new<T: Memory>(mem: &'a T, entry: u16) -> Cpu<'a> {
        Cpu {
            pc: entry,
            s: 0x00,
            a: 0x00,
            x: 0x00,
            y: 0x00,
            flags: 0b00100100u8,
            mem: mem,
            cycles: 0,
            irq: false,
            nmi: false,
            reset: false,
        }
    }

    pub fn tick(&mut self) {
        if self.cycles == 0 {
            let op = self.mem.get(self.pc);
            self.exec(op);
        } else {
            self.cycles -= 1
        }
    }

    pub fn exec(&mut self, op: u8) {
        match op {
            0x00 => self.call(ops::brk, ops::modes::implied),
            0x01 => self.call(ops::ora, ops::modes::xindirect),

            0x03 => self.call(ops::slo, ops::modes::xindirect),
            0x04 => self.call(ops::nop, ops::modes::zeropage),

            0x05 => self.call(ops::ora, ops::modes::zeropage),
            0x06 => self.call(ops::asl, ops::modes::zeropage),
            0x07 => self.call(ops::slo, ops::modes::zeropage),
            0x08 => self.call(ops::php, ops::modes::implied),
            0x09 => self.call(ops::ora, ops::modes::immediate),
            0x0a => self.call(ops::asl, ops::modes::accumulator),

            0x0c => self.call(ops::nop, ops::modes::absolute),
            0x0d => self.call(ops::ora, ops::modes::absolute),
            0x0e => self.call(ops::asl, ops::modes::absolute),
            0x0f => self.call(ops::slo, ops::modes::absolute),
            0x10 => self.call(ops::bpl, ops::modes::relative),
            0x11 => self.call(ops::ora, ops::modes::indirecty),

            0x13 => self.call(ops::slo, ops::modes::indirecty),
            0x14 => self.call(ops::nop, ops::modes::zeropagex),
            0x15 => self.call(ops::ora, ops::modes::zeropagex),
            0x16 => self.call(ops::asl, ops::modes::zeropagex),
            0x17 => self.call(ops::slo, ops::modes::zeropagex),
            0x18 => self.call(ops::clc, ops::modes::implied),
            0x19 => self.call(ops::ora, ops::modes::absolutey),
            0x1a => self.call(ops::nop, ops::modes::implied),
            0x1b => self.call(ops::slo, ops::modes::absolutey),
            0x1c => self.call(ops::nop, ops::modes::absolutex),
            0x1d => self.call(ops::ora, ops::modes::absolutex),

            0x1e => self.call(ops::asl, ops::modes::absolutex),
            0x1f => self.call(ops::slo, ops::modes::absolutex),
            0x20 => self.call(ops::jsr, ops::modes::absolute),
            0x21 => self.call(ops::and, ops::modes::xindirect),

            0x23 => self.call(ops::rla, ops::modes::xindirect),
            0x24 => self.call(ops::bit, ops::modes::zeropage),
            0x25 => self.call(ops::and, ops::modes::zeropage),
            0x26 => self.call(ops::rol, ops::modes::zeropage),
            0x27 => self.call(ops::rla, ops::modes::zeropage),
            0x28 => self.call(ops::plp, ops::modes::implied),
            0x29 => self.call(ops::and, ops::modes::immediate),
            0x2a => self.call(ops::rol, ops::modes::accumulator),

            0x2c => self.call(ops::bit, ops::modes::absolute),
            0x2d => self.call(ops::and, ops::modes::absolute),
            0x2e => self.call(ops::rol, ops::modes::absolute),
            0x2f => self.call(ops::rla, ops::modes::absolute),
            0x30 => self.call(ops::bmi, ops::modes::relative),
            0x31 => self.call(ops::and, ops::modes::indirecty),

            0x33 => self.call(ops::rla, ops::modes::indirecty),
            0x34 => self.call(ops::nop, ops::modes::zeropagex),
            0x35 => self.call(ops::and, ops::modes::zeropagex),
            0x36 => self.call(ops::rol, ops::modes::zeropagex),
            0x37 => self.call(ops::rla, ops::modes::zeropagex),
            0x38 => self.call(ops::sec, ops::modes::implied),
            0x39 => self.call(ops::and, ops::modes::absolutey),
            0x3a => self.call(ops::nop, ops::modes::implied),
            0x3b => self.call(ops::rla, ops::modes::absolutey),
            0x3c => self.call(ops::nop, ops::modes::absolutex),
            0x3d => self.call(ops::and, ops::modes::absolutex),
            0x3e => self.call(ops::rol, ops::modes::absolutex),
            0x3f => self.call(ops::rla, ops::modes::absolutex),
            0x40 => self.call(ops::rti, ops::modes::implied),
            0x41 => self.call(ops::eor, ops::modes::xindirect),

            0x43 => self.call(ops::sre, ops::modes::xindirect),
            0x44 => self.call(ops::nop, ops::modes::zeropage),
            0x45 => self.call(ops::eor, ops::modes::zeropage),
            0x46 => self.call(ops::lsr, ops::modes::zeropage),
            0x47 => self.call(ops::sre, ops::modes::zeropage),
            0x48 => self.call(ops::pha, ops::modes::implied),
            0x49 => self.call(ops::eor, ops::modes::immediate),
            0x4a => self.call(ops::lsr, ops::modes::accumulator),

            0x4c => self.call(ops::jmp, ops::modes::absolute),
            0x4d => self.call(ops::eor, ops::modes::absolute),
            0x4e => self.call(ops::lsr, ops::modes::absolute),
            0x4f => self.call(ops::sre, ops::modes::absolute),
            0x50 => self.call(ops::bvc, ops::modes::relative),
            0x51 => self.call(ops::eor, ops::modes::indirecty),

            0x53 => self.call(ops::sre, ops::modes::indirecty),
            0x54 => self.call(ops::nop, ops::modes::zeropagex),
            0x55 => self.call(ops::eor, ops::modes::zeropagex),
            0x56 => self.call(ops::lsr, ops::modes::zeropagex),
            0x57 => self.call(ops::sre, ops::modes::zeropagex),
            0x58 => self.call(ops::cli, ops::modes::implied),
            0x59 => self.call(ops::eor, ops::modes::absolutey),
            0x5a => self.call(ops::nop, ops::modes::implied),
            0x5b => self.call(ops::sre, ops::modes::absolutey),
            0x5c => self.call(ops::nop, ops::modes::absolutex),
            0x5d => self.call(ops::eor, ops::modes::absolutex),
            0x5e => self.call(ops::lsr, ops::modes::absolutex),
            0x5f => self.call(ops::sre, ops::modes::absolutex),
            0x60 => self.call(ops::rts, ops::modes::implied),
            0x61 => self.call(ops::adc, ops::modes::xindirect),

            0x63 => self.call(ops::rra, ops::modes::xindirect),
            0x64 => self.call(ops::nop, ops::modes::zeropagex),
            0x65 => self.call(ops::adc, ops::modes::zeropage),
            0x66 => self.call(ops::ror, ops::modes::zeropage),
            0x67 => self.call(ops::rra, ops::modes::zeropage),
            0x68 => self.call(ops::pla, ops::modes::implied),
            0x69 => self.call(ops::adc, ops::modes::immediate),
            0x6a => self.call(ops::ror, ops::modes::accumulator),

            0x6c => self.call(ops::jmp, ops::modes::indirect),
            0x6d => self.call(ops::adc, ops::modes::absolute),
            0x6e => self.call(ops::ror, ops::modes::absolute),
            0x6f => self.call(ops::rra, ops::modes::absolute),
            0x70 => self.call(ops::bvs, ops::modes::relative),
            0x71 => self.call(ops::adc, ops::modes::indirecty),

            0x73 => self.call(ops::rra, ops::modes::indirecty),
            0x74 => self.call(ops::nop, ops::modes::zeropagex),
            0x75 => self.call(ops::adc, ops::modes::zeropagex),
            0x76 => self.call(ops::ror, ops::modes::zeropagex),
            0x77 => self.call(ops::rra, ops::modes::zeropagex),
            0x78 => self.call(ops::sei, ops::modes::implied),
            0x79 => self.call(ops::adc, ops::modes::absolutey),
            0x7a => self.call(ops::nop, ops::modes::implied),
            0x7b => self.call(ops::rra, ops::modes::absolutey),
            0x7c => self.call(ops::nop, ops::modes::absolutex),
            0x7d => self.call(ops::adc, ops::modes::absolutex),
            0x7e => self.call(ops::ror, ops::modes::absolutex),
            0x7f => self.call(ops::rra, ops::modes::absolutex),
            0x80 => self.call(ops::nop, ops::modes::immediate),
            0x81 => self.call(ops::sta, ops::modes::xindirect),
            0x82 => self.call(ops::nop, ops::modes::immediate),
            0x83 => self.call(ops::sax, ops::modes::xindirect),
            0x84 => self.call(ops::sty, ops::modes::zeropage),
            0x85 => self.call(ops::sta, ops::modes::zeropage),
            0x86 => self.call(ops::stx, ops::modes::zeropage),
            0x87 => self.call(ops::sax, ops::modes::zeropage),
            0x88 => self.call(ops::dey, ops::modes::implied),
            0x89 => self.call(ops::nop, ops::modes::immediate),

            0x8a => self.call(ops::txa, ops::modes::implied),

            0x8c => self.call(ops::sty, ops::modes::absolute),
            0x8d => self.call(ops::sta, ops::modes::absolute),
            0x8e => self.call(ops::stx, ops::modes::absolute),
            0x8f => self.call(ops::sax, ops::modes::absolute),

            0x90 => self.call(ops::bcc, ops::modes::relative),
            0x91 => self.call(ops::sta, ops::modes::indirecty),

            0x94 => self.call(ops::sty, ops::modes::zeropagex),
            0x95 => self.call(ops::sta, ops::modes::zeropagex),
            0x96 => self.call(ops::stx, ops::modes::zeropagey),
            0x97 => self.call(ops::sax, ops::modes::zeropagey),
            0x98 => self.call(ops::tya, ops::modes::implied),
            0x99 => self.call(ops::sta, ops::modes::absolutey),
            0x9a => self.call(ops::txs, ops::modes::implied),

            0x9d => self.call(ops::sta, ops::modes::absolutex),

            0xa0 => self.call(ops::ldy, ops::modes::immediate),
            0xa1 => self.call(ops::lda, ops::modes::xindirect),
            0xa2 => self.call(ops::ldx, ops::modes::immediate),
            0xa3 => self.call(ops::lax, ops::modes::xindirect),
            0xa4 => self.call(ops::ldy, ops::modes::zeropage),
            0xa5 => self.call(ops::lda, ops::modes::zeropage),
            0xa6 => self.call(ops::ldx, ops::modes::zeropage),
            0xa7 => self.call(ops::lax, ops::modes::zeropage),

            0xa8 => self.call(ops::tay, ops::modes::implied),
            0xa9 => self.call(ops::lda, ops::modes::immediate),
            0xaa => self.call(ops::tax, ops::modes::implied),
            0xab => self.call(ops::lax, ops::modes::immediate),

            0xac => self.call(ops::ldy, ops::modes::absolute),
            0xad => self.call(ops::lda, ops::modes::absolute),
            0xae => self.call(ops::ldx, ops::modes::absolute),
            0xaf => self.call(ops::lax, ops::modes::absolute),

            0xb0 => self.call(ops::bcs, ops::modes::relative),
            0xb1 => self.call(ops::lda, ops::modes::indirecty),

            0xb3 => self.call(ops::lax, ops::modes::indirecty),
            0xb4 => self.call(ops::ldy, ops::modes::zeropagex),
            0xb5 => self.call(ops::lda, ops::modes::zeropagex),
            0xb6 => self.call(ops::ldx, ops::modes::zeropagey),
            0xb7 => self.call(ops::lax, ops::modes::zeropagey),

            0xb8 => self.call(ops::clv, ops::modes::implied),
            0xb9 => self.call(ops::lda, ops::modes::absolutey),
            0xba => self.call(ops::tsx, ops::modes::implied),

            0xbc => self.call(ops::ldy, ops::modes::absolutex),
            0xbd => self.call(ops::lda, ops::modes::absolutex),
            0xbe => self.call(ops::ldx, ops::modes::absolutey),
            0xbf => self.call(ops::lax, ops::modes::absolutey),

            0xc0 => self.call(ops::cpy, ops::modes::immediate),
            0xc1 => self.call(ops::cmp, ops::modes::xindirect),
            0xc2 => self.call(ops::nop, ops::modes::immediate),
            0xc3 => self.call(ops::dcp, ops::modes::xindirect),
            0xc4 => self.call(ops::cpy, ops::modes::zeropage),
            0xc5 => self.call(ops::cmp, ops::modes::zeropage),
            0xc6 => self.call(ops::dec, ops::modes::zeropage),
            0xc7 => self.call(ops::dcp, ops::modes::zeropage),
            0xc8 => self.call(ops::iny, ops::modes::implied),
            0xc9 => self.call(ops::cmp, ops::modes::immediate),
            0xca => self.call(ops::dex, ops::modes::implied),

            0xcc => self.call(ops::cpy, ops::modes::absolute),
            0xcd => self.call(ops::cmp, ops::modes::absolute),
            0xce => self.call(ops::dec, ops::modes::absolute),
            0xcf => self.call(ops::dcp, ops::modes::absolute),
            0xd0 => self.call(ops::bne, ops::modes::relative),
            0xd1 => self.call(ops::cmp, ops::modes::indirecty),

            0xd3 => self.call(ops::dcp, ops::modes::indirecty),
            0xd4 => self.call(ops::nop, ops::modes::zeropagex),
            0xd5 => self.call(ops::cmp, ops::modes::zeropagex),
            0xd6 => self.call(ops::dec, ops::modes::zeropagex),
            0xd7 => self.call(ops::dcp, ops::modes::zeropagex),
            0xd8 => self.call(ops::cld, ops::modes::implied),
            0xd9 => self.call(ops::cmp, ops::modes::absolutey),
            0xda => self.call(ops::nop, ops::modes::implied),
            0xdb => self.call(ops::dcp, ops::modes::absolutey),
            0xdc => self.call(ops::nop, ops::modes::absolutex),
            0xdd => self.call(ops::cmp, ops::modes::absolutex),
            0xde => self.call(ops::dec, ops::modes::absolutex),
            0xdf => self.call(ops::dcp, ops::modes::absolutex),
            0xe0 => self.call(ops::cpx, ops::modes::immediate),
            0xe1 => self.call(ops::sbc, ops::modes::xindirect),
            0xe2 => self.call(ops::nop, ops::modes::immediate),
            0xe3 => self.call(ops::isc, ops::modes::xindirect),
            0xe4 => self.call(ops::cpx, ops::modes::zeropage),
            0xe5 => self.call(ops::sbc, ops::modes::zeropage),
            0xe6 => self.call(ops::inc, ops::modes::zeropage),
            0xe7 => self.call(ops::isc, ops::modes::zeropage),
            0xe8 => self.call(ops::inx, ops::modes::implied),
            0xe9 => self.call(ops::sbc, ops::modes::immediate),
            0xea => self.call(ops::nop, ops::modes::implied),
            0xeb => self.call(ops::sbc, ops::modes::immediate),
            0xec => self.call(ops::cpx, ops::modes::absolute),
            0xed => self.call(ops::sbc, ops::modes::absolute),
            0xee => self.call(ops::inc, ops::modes::absolute),
            0xef => self.call(ops::isc, ops::modes::absolute),
            0xf0 => self.call(ops::beq, ops::modes::relative),
            0xf1 => self.call(ops::sbc, ops::modes::indirecty),

            0xf3 => self.call(ops::isc, ops::modes::indirecty),
            0xf4 => self.call(ops::nop, ops::modes::zeropagex),
            0xf5 => self.call(ops::sbc, ops::modes::zeropagex),
            0xf6 => self.call(ops::inc, ops::modes::zeropagex),
            0xf7 => self.call(ops::isc, ops::modes::zeropagex),
            0xf8 => self.call(ops::sed, ops::modes::implied),
            0xf9 => self.call(ops::sbc, ops::modes::absolutey),
            0xfa => self.call(ops::nop, ops::modes::implied),
            0xfb => self.call(ops::isc, ops::modes::absolutex),
            0xfc => self.call(ops::nop, ops::modes::absolutex),
            0xfd => self.call(ops::sbc, ops::modes::absolutex),
            0xfe => self.call(ops::inc, ops::modes::absolutex),
            0xff => self.call(ops::isc, ops::modes::absolutex),
            _ => {
                println!("unknown opcode {:x}", self.mem.get(self.pc));
                panic!();
                // self.pc += 1;
            }
        }
    }

    fn call(
        &mut self,
        op: fn(&mut Cpu, &ops::modes::Data),
        mode: fn(&mut Cpu) -> ops::modes::Data,
    ) {
        let data = mode(self);
        op(self, &data);
    }

    pub fn send_irq(&mut self) {
        self.irq = true;
    }

    pub fn send_nmi(&mut self) {
        self.nmi = true;
    }

    pub fn send_reset(&mut self) {
        self.reset = true;
    }
}

impl<'a> Cpu<'a> {
    // stack
    fn push(&mut self, data: u8) {
        self.mem.set(0x0100 + self.s as u16, data);
        self.s = self.s.wrapping_sub(1)
    }

    fn pop(&mut self) -> u8 {
        self.s = self.s.wrapping_add(1);
        self.mem.get(0x0100 + self.s as u16)
    }

    // flags
    fn set_flag(&mut self, flag: u8, val: bool) {
        self.flags = (self.flags & !(1 << flag)) | ((val as u8) << flag)
    }

    fn get_flag(&self, flag: u8) -> bool {
        ((self.flags >> flag) & 1) == 1
    }

    // poll for interrupts
    fn poll_interrupt(&mut self) {
        if self.nmi {
            self.irq = false;
            self.nmi()
        } else if self.irq && !self.get_flag(2) {
            self.irq()
        } else if self.reset {
            self.reset()
        }
    }
}

impl<'a> Cpu<'a> {
    // interrupt handlers
    fn irq(&mut self) {
        self.irq = false;

        let p = self.flags;

        self.pc += 1;
        let pch = (self.pc >> 8) as u8;
        let pcl = (self.pc & 0xFF) as u8;

        self.push(pch);
        self.push(pcl);
        self.push(p);

        self.pc = (self.mem.get(0xFFFF) as u16) << 8 + self.mem.get(0xFFFE);
        self.set_flag(2, true)
    }

    fn nmi(&mut self) {
        self.nmi = false;

        let p = self.flags;

        self.pc += 2;
        let pch = (self.pc >> 8) as u8;
        let pcl = (self.pc % u8::max_value() as u16) as u8;

        self.push(pch);
        self.push(pcl);
        self.push(p);

        self.pc = (self.mem.get(0xFFFB) as u16) << 8 + self.mem.get(0xFFFA);
        self.set_flag(2, true)
    }

    fn reset(&mut self) {
        self.reset = false;

        self.s = self.s.wrapping_sub(1);
        self.pc = (self.mem.get(0xFFFD) as u16) << 8 + self.mem.get(0xFFFC);
    }
}
