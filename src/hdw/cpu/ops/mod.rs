pub use self::Ops::*;
pub mod modes;

pub mod Ops {
    use super::modes;
    use crate::hdw::cpu::Cpu;

    const FLAG_C: u8 = 0;
    const FLAG_Z: u8 = 1;
    const FLAG_I: u8 = 2;
    const FLAG_D: u8 = 3;
    const FLAG_B: u8 = 4;
    const FLAG_V: u8 = 6;
    const FLAG_N: u8 = 7;

    pub fn adc(cpu: &mut Cpu, data: &modes::Data) {
        let result = cpu.a as u16 + data.get(cpu) as u16 + cpu.get_flag(FLAG_C) as u16;

        let z = (result & 0xFF) == 0;
        let n = (result >> 7) & 1 == 1;
        let v = ((!(cpu.a ^ data.get(cpu)) & (cpu.a ^ (result & 0xFF) as u8)) >> 7) & 1 == 1;
        let c = result > 255;
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
        cpu.set_flag(FLAG_V, v);
        cpu.set_flag(FLAG_C, c);
        cpu.a = (result & 0xFF) as u8;
    }

    pub fn and(cpu: &mut Cpu, data: &modes::Data) {
        cpu.a = cpu.a & data.get(cpu);
        let z = cpu.a == 0;
        let n = (cpu.a >> 7) == 1;
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
    }

    pub fn asl(cpu: &mut Cpu, data: &modes::Data) {
        let result = data.get(cpu) << 1;
        let c = (data.get(cpu) >> 7) & 1 == 1;

        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(FLAG_C, c);
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
        data.set(cpu, (result & 0xff) as u8);
    }

    pub fn bcc(cpu: &mut Cpu, data: &modes::Data) {
        let flag = !cpu.get_flag(0);
        branch(cpu, data, flag);
    }

    pub fn bcs(cpu: &mut Cpu, data: &modes::Data) {
        let flag = cpu.get_flag(0);
        branch(cpu, data, flag);
    }

    pub fn beq(cpu: &mut Cpu, data: &modes::Data) {
        let flag = cpu.get_flag(1);
        branch(cpu, data, flag);
    }

    pub fn bit(cpu: &mut Cpu, data: &modes::Data) {
        let m = data.get(cpu);
        let result = cpu.a & m;

        let z = result == 0;
        let n = (m >> 7) == 1;
        let v = ((m >> 6) & 1) == 1;
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
        cpu.set_flag(FLAG_V, v);
    }

    pub fn bmi(cpu: &mut Cpu, data: &modes::Data) {
        let flag = cpu.get_flag(7);
        branch(cpu, data, flag);
    }

    pub fn bne(cpu: &mut Cpu, data: &modes::Data) {
        let flag = !cpu.get_flag(1);
        branch(cpu, data, flag);
    }

    pub fn bpl(cpu: &mut Cpu, data: &modes::Data) {
        let flag = !cpu.get_flag(7);
        branch(cpu, data, flag);
    }

    pub fn brk(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.set_flag(FLAG_B, true);
        cpu.irq();
    }

    pub fn bvc(cpu: &mut Cpu, data: &modes::Data) {
        let flag = !cpu.get_flag(6);
        branch(cpu, data, flag);
    }

    pub fn bvs(cpu: &mut Cpu, data: &modes::Data) {
        let flag = cpu.get_flag(6);
        branch(cpu, data, flag);
    }

    pub fn clc(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.set_flag(FLAG_C, false);
    }

    pub fn cld(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.set_flag(FLAG_D, false);
    }

    pub fn cli(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.set_flag(FLAG_I, false);
    }

    pub fn clv(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.set_flag(FLAG_V, false);
    }

    pub fn cmp(cpu: &mut Cpu, data: &modes::Data) {
        let result = (cpu.a as u16).wrapping_sub(data.get(cpu) as u16);

        let z = result & 0xFF == 0;
        let n = (result >> 7) & 1 == 1;
        let c = cpu.a >= data.get(cpu);
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
        cpu.set_flag(FLAG_C, c);
    }

    pub fn cpx(cpu: &mut Cpu, data: &modes::Data) {
        let result = (cpu.x as u16).wrapping_sub(data.get(cpu) as u16);

        let z = result & 0xFF == 0;
        let n = (result >> 7) & 1 == 1;
        let c = cpu.x >= data.get(cpu);
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
        cpu.set_flag(FLAG_C, c);
    }

    pub fn cpy(cpu: &mut Cpu, data: &modes::Data) {
        let result = (cpu.y as u16).wrapping_sub(data.get(cpu) as u16);

        let z = result & 0xFF == 0;
        let n = (result >> 7) & 1 == 1;
        let c = cpu.y >= data.get(cpu);

        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
        cpu.set_flag(FLAG_C, c);
    }

    pub fn dcp(cpu: &mut Cpu, data: &modes::Data) {
        dec(cpu, data);
        cmp(cpu, data);
    }

    pub fn dec(cpu: &mut Cpu, data: &modes::Data) {
        let result = data.get(cpu).wrapping_sub(1);

        cpu.set_flag(FLAG_Z, result == 0);
        cpu.set_flag(FLAG_N, result >> 7 == 1);

        data.set(cpu, result);
    }

    pub fn dex(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.x = cpu.x.wrapping_sub(1);

        cpu.set_flag(FLAG_Z, cpu.x == 0);
        cpu.set_flag(FLAG_N, cpu.x >> 7 == 1);
    }

    pub fn dey(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.y = cpu.y.wrapping_sub(1);

        cpu.set_flag(FLAG_Z, cpu.y == 0);
        cpu.set_flag(FLAG_N, cpu.y >> 7 == 1);
    }

    pub fn eor(cpu: &mut Cpu, data: &modes::Data) {
        cpu.a = cpu.a ^ data.get(cpu);
        let z = cpu.a == 0;
        let n = (cpu.a >> 7) == 1;
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
    }

    pub fn inc(cpu: &mut Cpu, data: &modes::Data) {
        let result = data.get(cpu).wrapping_add(1);
        cpu.set_flag(FLAG_Z, result == 0);
        cpu.set_flag(FLAG_N, result >> 7 == 1);

        data.set(cpu, result);
    }

    pub fn inx(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.x = cpu.x.wrapping_add(1);

        cpu.set_flag(FLAG_Z, cpu.x == 0);
        cpu.set_flag(FLAG_N, cpu.x >> 7 == 1);
    }

    pub fn iny(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.y = cpu.y.wrapping_add(1);

        cpu.set_flag(FLAG_Z, cpu.y == 0);
        cpu.set_flag(FLAG_N, cpu.y >> 7 == 1);
    }

    pub fn isc(cpu: &mut Cpu, data: &modes::Data) { // undocumented
        inc(cpu, data);
        sbc(cpu, data);
    }

    pub fn jmp(cpu: &mut Cpu, data: &modes::Data) {
        match *data {
            modes::Data::Memory(address) => {
                cpu.pc = address;
            },
            _ => {}
        }
    }

    pub fn jsr(cpu: &mut Cpu, data: &modes::Data) {
        match *data {
            modes::Data::Memory(a) => {
                cpu.pc = cpu.pc.wrapping_sub(1);
                let lo = (cpu.pc % 256) as u8;
                let hi = (cpu.pc >> 8 % 256) as u8;
                cpu.push(hi);
                cpu.push(lo);
                cpu.pc = a;
            },
            _ => unreachable!(),
        };
    }

    pub fn lax(cpu: &mut Cpu, data: &modes::Data) {
        // undocumented
        let result = data.get(cpu);
        cpu.a = result;
        cpu.x = result;

        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
    }

    pub fn lda(cpu: &mut Cpu, data: &modes::Data) {
        let result = data.get(cpu);
        cpu.a = result;

        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
    }

    pub fn ldx(cpu: &mut Cpu, data: &modes::Data) {
        let result = data.get(cpu);
        cpu.x = result;

        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
    }

    pub fn ldy(cpu: &mut Cpu, data: &modes::Data) {
        let result = data.get(cpu);
        cpu.y = result;

        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
    }

    pub fn lsr(cpu: &mut Cpu, data: &modes::Data) {
        let result = data.get(cpu) >> 1;
        let c = data.get(cpu) & 1 == 1;

        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(FLAG_C, c);
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
        data.set(cpu, result);
    }

    pub fn nop(_cpu: &mut Cpu, _data: &modes::Data) {}

    pub fn ora(cpu: &mut Cpu, data: &modes::Data) {
        cpu.a = cpu.a | data.get(cpu);
        let z = cpu.a == 0;
        let n = (cpu.a >> 7) == 1;
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
    }

    pub fn pha(cpu: &mut Cpu, _data: &modes::Data) {
        let value = cpu.a;
        cpu.push(value);
    }

    pub fn php(cpu: &mut Cpu, _data: &modes::Data) {
        let result = cpu.flags | 0b0010000;
        cpu.push(result);
    }

    pub fn pla(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.a = cpu.pop();
        cpu.set_flag(FLAG_Z, cpu.a == 0);
        cpu.set_flag(FLAG_N, (cpu.a >> 7) & 1 == 1);
    }

    pub fn plp(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.flags = (cpu.pop() & 0b11001111) | 0b00100000;
    }

    pub fn rla(cpu: &mut Cpu, data: &modes::Data) {
        rol(cpu, data);
        and(cpu, data);
    }

    pub fn rol(cpu: &mut Cpu, data: &modes::Data) {
        let result = data.get(cpu) << 1 | cpu.get_flag(FLAG_C) as u8;
        let c = (data.get(cpu) >> 7) & 1 == 1;

        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(FLAG_C, c);
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
        data.set(cpu, result);
    }

    pub fn ror(cpu: &mut Cpu, data: &modes::Data) {
        let result = data.get(cpu) >> 1 | (cpu.get_flag(FLAG_C) as u8) << 7;
        let c = data.get(cpu) & 1 == 1;

        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(FLAG_C, c);
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
        data.set(cpu, result);
    }

    pub fn rra(cpu: &mut Cpu, data: &modes::Data) {
        ror(cpu, data);
        adc(cpu, data);
    }

    pub fn rti(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.flags = (cpu.pop() & 0b11001111) | 0b00100000;
        let lo = cpu.pop();
        let hi = cpu.pop();
        cpu.pc = ((hi as u16) << 8) + lo as u16;
    }

    pub fn rts(cpu: &mut Cpu, _data: &modes::Data) {
        let lo = cpu.pop();
        let hi = cpu.pop();
        cpu.pc = (((hi as u16) << 8) + lo as u16).wrapping_add(1);
    }

    pub fn sax(cpu: &mut Cpu, data: &modes::Data) {
        // undocumented
        match data {
            &modes::Data::Memory(a) => {
                cpu.mem.set(a, cpu.a & cpu.x);
            }
            _ => unreachable!(),
        }
    }

    pub fn sbc(cpu: &mut Cpu, data: &modes::Data) {
        let result = cpu.a as u16 + !data.get(cpu) as u16 + cpu.get_flag(FLAG_C) as u16;

        let z = (result & 0xFF) == 0;
        let n = (result >> 7) & 1 == 1;
        let v = ((!(cpu.a ^ !data.get(cpu)) & (cpu.a ^ (result & 0xFF) as u8)) >> 7) & 1 == 1;
        let c = result > 255;
        cpu.set_flag(FLAG_Z, z);
        cpu.set_flag(FLAG_N, n);
        cpu.set_flag(FLAG_V, v);
        cpu.set_flag(FLAG_C, c);
        cpu.a = (result & 0xFF) as u8;
    }

    pub fn sec(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.set_flag(FLAG_C, true);
    }

    pub fn sed(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.set_flag(FLAG_D, true);
    }

    pub fn sei(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.set_flag(FLAG_I, true);
    }

    pub fn sta(cpu: &mut Cpu, data: &modes::Data) {
        match data {
            &modes::Data::Memory(a) => {
                cpu.mem.set(a, cpu.a);
            }
            _ => unreachable!(),
        }
    }

    pub fn stx(cpu: &mut Cpu, data: &modes::Data) {
        match data {
            &modes::Data::Memory(a) => {
                cpu.mem.set(a, cpu.x);
            }
            _ => unreachable!(),
        }
    }

    pub fn sty(cpu: &mut Cpu, data: &modes::Data) {
        match data {
            &modes::Data::Memory(a) => {
                cpu.mem.set(a, cpu.y);
            }
            _ => unreachable!(),
        }
    }

    pub fn slo(cpu: &mut Cpu, data: &modes::Data) { // undocumented
        asl(cpu, data);
        ora(cpu, data);
    }

    pub fn sre(cpu: &mut Cpu, data: &modes::Data) {
        lsr(cpu, data);
        eor(cpu, data);
    }

    pub fn tax(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.x = cpu.a;

        cpu.set_flag(FLAG_Z, cpu.x == 0);
        cpu.set_flag(FLAG_N, cpu.x >> 7 == 1);
    }

    pub fn tay(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.y = cpu.a;

        cpu.set_flag(FLAG_Z, cpu.y == 0);
        cpu.set_flag(FLAG_N, cpu.y >> 7 == 1);
    }

    pub fn tsx(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.x = cpu.s;

        cpu.set_flag(FLAG_Z, cpu.x == 0);
        cpu.set_flag(FLAG_N, cpu.x >> 7 == 1);
    }

    pub fn txa(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.a = cpu.x;

        cpu.set_flag(FLAG_Z, cpu.a == 0);
        cpu.set_flag(FLAG_N, cpu.a >> 7 == 1);
    }

    pub fn txs(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.s = cpu.x;
    }

    pub fn tya(cpu: &mut Cpu, _data: &modes::Data) {
        cpu.a = cpu.y;

        cpu.set_flag(FLAG_Z, cpu.a == 0);
        cpu.set_flag(FLAG_N, cpu.a >> 7 == 1);
    }

    fn branch(cpu: &mut Cpu, data: &modes::Data, flag: bool) {
        if flag {
            match data {
                &modes::Data::Memory(a) => cpu.pc = a,
                _ => {}
            }
        }
    }
}
