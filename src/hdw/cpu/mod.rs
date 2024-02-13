use crate::hdw::Memory;

mod ops;
mod modes;

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
            mem,
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
            0x00 => self.call(ops::brk, modes::implied),
            0x01 => self.call(ops::ora, modes::xindirect),

            0x03 => self.call(ops::slo, modes::xindirect),
            0x04 => self.call(ops::nop, modes::zeropage),

            0x05 => self.call(ops::ora, modes::zeropage),
            0x06 => self.call(ops::asl, modes::zeropage),
            0x07 => self.call(ops::slo, modes::zeropage),
            0x08 => self.call(ops::php, modes::implied),
            0x09 => self.call(ops::ora, modes::immediate),
            0x0a => self.call(ops::asl, modes::accumulator),

            0x0c => self.call(ops::nop, modes::absolute),
            0x0d => self.call(ops::ora, modes::absolute),
            0x0e => self.call(ops::asl, modes::absolute),
            0x0f => self.call(ops::slo, modes::absolute),
            0x10 => self.call(ops::bpl, modes::relative),
            0x11 => self.call(ops::ora, modes::indirecty),

            0x13 => self.call(ops::slo, modes::indirecty),
            0x14 => self.call(ops::nop, modes::zeropagex),
            0x15 => self.call(ops::ora, modes::zeropagex),
            0x16 => self.call(ops::asl, modes::zeropagex),
            0x17 => self.call(ops::slo, modes::zeropagex),
            0x18 => self.call(ops::clc, modes::implied),
            0x19 => self.call(ops::ora, modes::absolutey),
            0x1a => self.call(ops::nop, modes::implied),
            0x1b => self.call(ops::slo, modes::absolutey),
            0x1c => self.call(ops::nop, modes::absolutex),
            0x1d => self.call(ops::ora, modes::absolutex),

            0x1e => self.call(ops::asl, modes::absolutex),
            0x1f => self.call(ops::slo, modes::absolutex),
            0x20 => self.call(ops::jsr, modes::absolute),
            0x21 => self.call(ops::and, modes::xindirect),

            0x23 => self.call(ops::rla, modes::xindirect),
            0x24 => self.call(ops::bit, modes::zeropage),
            0x25 => self.call(ops::and, modes::zeropage),
            0x26 => self.call(ops::rol, modes::zeropage),
            0x27 => self.call(ops::rla, modes::zeropage),
            0x28 => self.call(ops::plp, modes::implied),
            0x29 => self.call(ops::and, modes::immediate),
            0x2a => self.call(ops::rol, modes::accumulator),

            0x2c => self.call(ops::bit, modes::absolute),
            0x2d => self.call(ops::and, modes::absolute),
            0x2e => self.call(ops::rol, modes::absolute),
            0x2f => self.call(ops::rla, modes::absolute),
            0x30 => self.call(ops::bmi, modes::relative),
            0x31 => self.call(ops::and, modes::indirecty),

            0x33 => self.call(ops::rla, modes::indirecty),
            0x34 => self.call(ops::nop, modes::zeropagex),
            0x35 => self.call(ops::and, modes::zeropagex),
            0x36 => self.call(ops::rol, modes::zeropagex),
            0x37 => self.call(ops::rla, modes::zeropagex),
            0x38 => self.call(ops::sec, modes::implied),
            0x39 => self.call(ops::and, modes::absolutey),
            0x3a => self.call(ops::nop, modes::implied),
            0x3b => self.call(ops::rla, modes::absolutey),
            0x3c => self.call(ops::nop, modes::absolutex),
            0x3d => self.call(ops::and, modes::absolutex),
            0x3e => self.call(ops::rol, modes::absolutex),
            0x3f => self.call(ops::rla, modes::absolutex),
            0x40 => self.call(ops::rti, modes::implied),
            0x41 => self.call(ops::eor, modes::xindirect),

            0x43 => self.call(ops::sre, modes::xindirect),
            0x44 => self.call(ops::nop, modes::zeropage),
            0x45 => self.call(ops::eor, modes::zeropage),
            0x46 => self.call(ops::lsr, modes::zeropage),
            0x47 => self.call(ops::sre, modes::zeropage),
            0x48 => self.call(ops::pha, modes::implied),
            0x49 => self.call(ops::eor, modes::immediate),
            0x4a => self.call(ops::lsr, modes::accumulator),

            0x4c => self.call(ops::jmp, modes::absolute),
            0x4d => self.call(ops::eor, modes::absolute),
            0x4e => self.call(ops::lsr, modes::absolute),
            0x4f => self.call(ops::sre, modes::absolute),
            0x50 => self.call(ops::bvc, modes::relative),
            0x51 => self.call(ops::eor, modes::indirecty),

            0x53 => self.call(ops::sre, modes::indirecty),
            0x54 => self.call(ops::nop, modes::zeropagex),
            0x55 => self.call(ops::eor, modes::zeropagex),
            0x56 => self.call(ops::lsr, modes::zeropagex),
            0x57 => self.call(ops::sre, modes::zeropagex),
            0x58 => self.call(ops::cli, modes::implied),
            0x59 => self.call(ops::eor, modes::absolutey),
            0x5a => self.call(ops::nop, modes::implied),
            0x5b => self.call(ops::sre, modes::absolutey),
            0x5c => self.call(ops::nop, modes::absolutex),
            0x5d => self.call(ops::eor, modes::absolutex),
            0x5e => self.call(ops::lsr, modes::absolutex),
            0x5f => self.call(ops::sre, modes::absolutex),
            0x60 => self.call(ops::rts, modes::implied),
            0x61 => self.call(ops::adc, modes::xindirect),

            0x63 => self.call(ops::rra, modes::xindirect),
            0x64 => self.call(ops::nop, modes::zeropagex),
            0x65 => self.call(ops::adc, modes::zeropage),
            0x66 => self.call(ops::ror, modes::zeropage),
            0x67 => self.call(ops::rra, modes::zeropage),
            0x68 => self.call(ops::pla, modes::implied),
            0x69 => self.call(ops::adc, modes::immediate),
            0x6a => self.call(ops::ror, modes::accumulator),

            0x6c => self.call(ops::jmp, modes::indirect),
            0x6d => self.call(ops::adc, modes::absolute),
            0x6e => self.call(ops::ror, modes::absolute),
            0x6f => self.call(ops::rra, modes::absolute),
            0x70 => self.call(ops::bvs, modes::relative),
            0x71 => self.call(ops::adc, modes::indirecty),

            0x73 => self.call(ops::rra, modes::indirecty),
            0x74 => self.call(ops::nop, modes::zeropagex),
            0x75 => self.call(ops::adc, modes::zeropagex),
            0x76 => self.call(ops::ror, modes::zeropagex),
            0x77 => self.call(ops::rra, modes::zeropagex),
            0x78 => self.call(ops::sei, modes::implied),
            0x79 => self.call(ops::adc, modes::absolutey),
            0x7a => self.call(ops::nop, modes::implied),
            0x7b => self.call(ops::rra, modes::absolutey),
            0x7c => self.call(ops::nop, modes::absolutex),
            0x7d => self.call(ops::adc, modes::absolutex),
            0x7e => self.call(ops::ror, modes::absolutex),
            0x7f => self.call(ops::rra, modes::absolutex),
            0x80 => self.call(ops::nop, modes::immediate),
            0x81 => self.call(ops::sta, modes::xindirect),
            0x82 => self.call(ops::nop, modes::immediate),
            0x83 => self.call(ops::sax, modes::xindirect),
            0x84 => self.call(ops::sty, modes::zeropage),
            0x85 => self.call(ops::sta, modes::zeropage),
            0x86 => self.call(ops::stx, modes::zeropage),
            0x87 => self.call(ops::sax, modes::zeropage),
            0x88 => self.call(ops::dey, modes::implied),
            0x89 => self.call(ops::nop, modes::immediate),

            0x8a => self.call(ops::txa, modes::implied),

            0x8c => self.call(ops::sty, modes::absolute),
            0x8d => self.call(ops::sta, modes::absolute),
            0x8e => self.call(ops::stx, modes::absolute),
            0x8f => self.call(ops::sax, modes::absolute),

            0x90 => self.call(ops::bcc, modes::relative),
            0x91 => self.call(ops::sta, modes::indirecty),

            0x94 => self.call(ops::sty, modes::zeropagex),
            0x95 => self.call(ops::sta, modes::zeropagex),
            0x96 => self.call(ops::stx, modes::zeropagey),
            0x97 => self.call(ops::sax, modes::zeropagey),
            0x98 => self.call(ops::tya, modes::implied),
            0x99 => self.call(ops::sta, modes::absolutey),
            0x9a => self.call(ops::txs, modes::implied),

            0x9d => self.call(ops::sta, modes::absolutex),

            0xa0 => self.call(ops::ldy, modes::immediate),
            0xa1 => self.call(ops::lda, modes::xindirect),
            0xa2 => self.call(ops::ldx, modes::immediate),
            0xa3 => self.call(ops::lax, modes::xindirect),
            0xa4 => self.call(ops::ldy, modes::zeropage),
            0xa5 => self.call(ops::lda, modes::zeropage),
            0xa6 => self.call(ops::ldx, modes::zeropage),
            0xa7 => self.call(ops::lax, modes::zeropage),

            0xa8 => self.call(ops::tay, modes::implied),
            0xa9 => self.call(ops::lda, modes::immediate),
            0xaa => self.call(ops::tax, modes::implied),
            0xab => self.call(ops::lax, modes::immediate),

            0xac => self.call(ops::ldy, modes::absolute),
            0xad => self.call(ops::lda, modes::absolute),
            0xae => self.call(ops::ldx, modes::absolute),
            0xaf => self.call(ops::lax, modes::absolute),

            0xb0 => self.call(ops::bcs, modes::relative),
            0xb1 => self.call(ops::lda, modes::indirecty),

            0xb3 => self.call(ops::lax, modes::indirecty),
            0xb4 => self.call(ops::ldy, modes::zeropagex),
            0xb5 => self.call(ops::lda, modes::zeropagex),
            0xb6 => self.call(ops::ldx, modes::zeropagey),
            0xb7 => self.call(ops::lax, modes::zeropagey),

            0xb8 => self.call(ops::clv, modes::implied),
            0xb9 => self.call(ops::lda, modes::absolutey),
            0xba => self.call(ops::tsx, modes::implied),

            0xbc => self.call(ops::ldy, modes::absolutex),
            0xbd => self.call(ops::lda, modes::absolutex),
            0xbe => self.call(ops::ldx, modes::absolutey),
            0xbf => self.call(ops::lax, modes::absolutey),

            0xc0 => self.call(ops::cpy, modes::immediate),
            0xc1 => self.call(ops::cmp, modes::xindirect),
            0xc2 => self.call(ops::nop, modes::immediate),
            0xc3 => self.call(ops::dcp, modes::xindirect),
            0xc4 => self.call(ops::cpy, modes::zeropage),
            0xc5 => self.call(ops::cmp, modes::zeropage),
            0xc6 => self.call(ops::dec, modes::zeropage),
            0xc7 => self.call(ops::dcp, modes::zeropage),
            0xc8 => self.call(ops::iny, modes::implied),
            0xc9 => self.call(ops::cmp, modes::immediate),
            0xca => self.call(ops::dex, modes::implied),

            0xcc => self.call(ops::cpy, modes::absolute),
            0xcd => self.call(ops::cmp, modes::absolute),
            0xce => self.call(ops::dec, modes::absolute),
            0xcf => self.call(ops::dcp, modes::absolute),
            0xd0 => self.call(ops::bne, modes::relative),
            0xd1 => self.call(ops::cmp, modes::indirecty),

            0xd3 => self.call(ops::dcp, modes::indirecty),
            0xd4 => self.call(ops::nop, modes::zeropagex),
            0xd5 => self.call(ops::cmp, modes::zeropagex),
            0xd6 => self.call(ops::dec, modes::zeropagex),
            0xd7 => self.call(ops::dcp, modes::zeropagex),
            0xd8 => self.call(ops::cld, modes::implied),
            0xd9 => self.call(ops::cmp, modes::absolutey),
            0xda => self.call(ops::nop, modes::implied),
            0xdb => self.call(ops::dcp, modes::absolutey),
            0xdc => self.call(ops::nop, modes::absolutex),
            0xdd => self.call(ops::cmp, modes::absolutex),
            0xde => self.call(ops::dec, modes::absolutex),
            0xdf => self.call(ops::dcp, modes::absolutex),
            0xe0 => self.call(ops::cpx, modes::immediate),
            0xe1 => self.call(ops::sbc, modes::xindirect),
            0xe2 => self.call(ops::nop, modes::immediate),
            0xe3 => self.call(ops::isc, modes::xindirect),
            0xe4 => self.call(ops::cpx, modes::zeropage),
            0xe5 => self.call(ops::sbc, modes::zeropage),
            0xe6 => self.call(ops::inc, modes::zeropage),
            0xe7 => self.call(ops::isc, modes::zeropage),
            0xe8 => self.call(ops::inx, modes::implied),
            0xe9 => self.call(ops::sbc, modes::immediate),
            0xea => self.call(ops::nop, modes::implied),
            0xeb => self.call(ops::sbc, modes::immediate),
            0xec => self.call(ops::cpx, modes::absolute),
            0xed => self.call(ops::sbc, modes::absolute),
            0xee => self.call(ops::inc, modes::absolute),
            0xef => self.call(ops::isc, modes::absolute),
            0xf0 => self.call(ops::beq, modes::relative),
            0xf1 => self.call(ops::sbc, modes::indirecty),

            0xf3 => self.call(ops::isc, modes::indirecty),
            0xf4 => self.call(ops::nop, modes::zeropagex),
            0xf5 => self.call(ops::sbc, modes::zeropagex),
            0xf6 => self.call(ops::inc, modes::zeropagex),
            0xf7 => self.call(ops::isc, modes::zeropagex),
            0xf8 => self.call(ops::sed, modes::implied),
            0xf9 => self.call(ops::sbc, modes::absolutey),
            0xfa => self.call(ops::nop, modes::implied),
            0xfb => self.call(ops::isc, modes::absolutex),
            0xfc => self.call(ops::nop, modes::absolutex),
            0xfd => self.call(ops::sbc, modes::absolutex),
            0xfe => self.call(ops::inc, modes::absolutex),
            0xff => self.call(ops::isc, modes::absolutex),
            _ => {
                println!("unknown opcode {:x}", self.mem.get(self.pc));
                panic!();
                // self.pc += 1;
            }
        }
    }

    fn call(
        &mut self,
        op: fn(&mut Cpu, &modes::Data),
        mode: fn(&mut Cpu) -> modes::Data,
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

        self.pc = ((self.mem.get(0xFFFF) as u16) << 8) + self.mem.get(0xFFFE) as u16;
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

        self.pc = ((self.mem.get(0xFFFB) as u16) << 8) + self.mem.get(0xFFFA) as u16;
        self.set_flag(2, true)
    }

    fn reset(&mut self) {
        self.reset = false;

        self.s = self.s.wrapping_sub(1);
        self.pc = ((self.mem.get(0xFFFD) as u16) << 8) + self.mem.get(0xFFFC) as u16
    }
}
