use hdw::cpu::Cpu;
pub use self::Ops::*;
pub mod modes;

pub mod Ops {
    use hdw::cpu::Cpu;
    use super::modes;

    pub fn adc(cpu: &mut Cpu, data: modes::data) {
        let result = cpu.a as u16 + data.get(cpu) as u16 + cpu.get_flag(0) as u16;

        let z = result == 0;
        let n = (result >> 7) == 1;
        let v = (!((((result % 256) as u8) >> 7) ^ (cpu.a >> 7)) & (cpu.a ^ cpu.get_flag(0) as u8))
            == 1;
        let c = result > 255;
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);
        cpu.set_flag(6, v);
        cpu.set_flag(0, c);
        cpu.a = (result % 256) as u8;

        cpu.poll_interrupt();
    }

    pub fn and(cpu: &mut Cpu, data: modes::data) {
        cpu.a = cpu.a & data.get(cpu);
        let z = cpu.a == 0;
        let n = (cpu.a >> 7) == 1;
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);

        cpu.poll_interrupt();
    }

    pub fn asl(cpu: &mut Cpu, data: modes::data) {
        let result = (data.get(cpu) as u16) >> 1;
        let c = (data.get(cpu) & 1) == 1;

        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(0, c);
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);
        data.set(cpu, (result & 0xff) as u8);

        cpu.poll_interrupt();
    }

    pub fn bcc(cpu: &mut Cpu, data: modes::data) {
        let flag = !cpu.get_flag(0);
        branch(cpu, data, flag);
    }
    
    pub fn bcs(cpu: &mut Cpu, data: modes::data) {
        let flag = cpu.get_flag(0);
        branch(cpu, data, flag);
    }
    
    pub fn beq(cpu: &mut Cpu, data: modes::data) {
        let flag = cpu.get_flag(1);
        branch(cpu, data, flag);
    }

    pub fn bit(cpu: &mut Cpu, data: modes::data) {
        let result = cpu.a & data.get(cpu);

        let z = result == 0;
        let n = (result >> 7) == 1;
        let v = ((result >> 6) & 1) == 1;
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);
        cpu.set_flag(6, v);
        
        cpu.poll_interrupt();
    }

    pub fn bmi(cpu: &mut Cpu, data: modes::data) {
        let flag = cpu.get_flag(7);
        branch(cpu, data, flag);
    }
    
    pub fn bne(cpu: &mut Cpu, data: modes::data) {
        let flag = !cpu.get_flag(1);
        branch(cpu, data, flag);
    }

    pub fn bpl(cpu: &mut Cpu, data: modes::data) {
        let flag = !cpu.get_flag(7);
    }

    pub fn brk(cpu: &mut Cpu, _data: modes::data) {
        cpu.set_flag(4, true);
        cpu.irq();

        cpu.poll_interrupt();
    }

    pub fn bvc(cpu: &mut Cpu, data: modes::data) {
        let flag = !cpu.get_flag(6);
        branch(cpu, data, flag);
    }

    pub fn bvs(cpu: &mut Cpu, data: modes::data) {
        let flag = cpu.get_flag(6);
        branch(cpu, data, flag);
    }

    pub fn clc(cpu: &mut Cpu, _data: modes::data) {
        cpu.set_flag(0, false);

        cpu.poll_interrupt();
    }

    pub fn cli(cpu: &mut Cpu, _data: modes::data) {
        cpu.set_flag(2, false);

        cpu.poll_interrupt();
    }
    
    pub fn clv(cpu: &mut Cpu, _data: modes::data) {
        cpu.set_flag(6, false);

        cpu.poll_interrupt();
    }
    
    pub fn cmp(cpu: &mut Cpu, data: modes::data) {
        let result = (cpu.a as u16).wrapping_sub(data.get(cpu) as u16);
        
        let z = result == 0;
        let n = (result >> 7) == 1;
        let c = cpu.a >= data.get(cpu);
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);
        cpu.set_flag(0, c);
        
        cpu.poll_interrupt();
    }
    
    pub fn cpx(cpu: &mut Cpu, data: modes::data) {
        let result = (cpu.x as u16).wrapping_add(!(data.get(cpu) as u16));

        let z = result == 0;
        let n = (result >> 7) == 1;
        let c = result > 255;
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);
        cpu.set_flag(0, c);
        
        cpu.poll_interrupt();
    }
    
    pub fn cpy(cpu: &mut Cpu, data: modes::data) {
        let result = (cpu.y as u16).wrapping_add(!(data.get(cpu) as u16));

        let z = result == 0;
        let n = (result >> 7) == 1;
        let c = result > 255;
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);
        cpu.set_flag(0, c);
        
        cpu.poll_interrupt();
    }
    
    pub fn dec(cpu: &mut Cpu, data: modes::data) {
        let result = data.get(cpu).wrapping_sub(1);
        
        data.set(cpu, result);

        cpu.poll_interrupt();
    }
    
    pub fn dex(cpu: &mut Cpu, _data: modes::data) {
        cpu.x = cpu.x.wrapping_sub(1);

        cpu.poll_interrupt();
    }

    pub fn dey(cpu: &mut Cpu, _data: modes::data) {
        cpu.y = cpu.y.wrapping_sub(1);

        cpu.poll_interrupt();
    }

    pub fn eor(cpu: &mut Cpu, data: modes::data) {
        cpu.a = cpu.a ^ data.get(cpu);
        let z = cpu.a == 0;
        let n = (cpu.a >> 7) == 1;
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);

        cpu.poll_interrupt();
    }
    
    pub fn inc(cpu: &mut Cpu, data: modes::data) {
        let result = data.get(cpu).wrapping_add(1);
        
        data.set(cpu, result);

        cpu.poll_interrupt();
    }
    
    pub fn inx(cpu: &mut Cpu, _data: modes::data) {
        cpu.x = cpu.x.wrapping_add(1);

        cpu.poll_interrupt();
    }
    
    pub fn iny(cpu: &mut Cpu, _data: modes::data) {
        cpu.y = cpu.y.wrapping_add(1);

        cpu.poll_interrupt();
    }
    
    pub fn jmp(cpu: &mut Cpu, data: modes::data) {
        match data {
            modes::data::Memory(address) => {
                cpu.pc = address;

                cpu.poll_interrupt();
            }
            _ => {}
        }
    }

    pub fn jsr(cpu: &mut Cpu, data: modes::data) {
        match data {
            modes::data::Memory(a) => {
                cpu.pc = cpu.pc.wrapping_sub(1);
                let lo = (cpu.pc.wrapping_add(2) % 256) as u8;
                let hi = (cpu.pc.wrapping_add(2) >> 8 % 256) as u8;
                cpu.push(hi);
                cpu.push(lo);
                cpu.pc = a;
            }
            _ => unreachable!(),
        };

        cpu.poll_interrupt();
    }

    pub fn lda(cpu: &mut Cpu, data: modes::data) {
        let result = data.get(cpu);
        cpu.a = result;
        
        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);

        cpu.poll_interrupt();
    }

    pub fn ldx(cpu: &mut Cpu, data: modes::data) {
        let result = data.get(cpu);
        cpu.x = result;
        
        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);

        cpu.poll_interrupt();
    }

    pub fn ldy(cpu: &mut Cpu, data: modes::data) {
        let result = data.get(cpu);
        cpu.y = result;
        
        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);

        cpu.poll_interrupt();
    }

    pub fn lsr(cpu: &mut Cpu, data: modes::data) {
        let result = (data.get(cpu) as u16) << 1;
        let c = (result >> 8) == 1;

        let z = result == 0;
        let n = (result >> 7) == 1;
        cpu.set_flag(0, c);
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);
        data.set(cpu, (result & 0xff) as u8);

        cpu.poll_interrupt();
    }

    pub fn ora(cpu: &mut Cpu, data: modes::data) {
        cpu.a = cpu.a | data.get(cpu);
        let z = cpu.a == 0;
        let n = (cpu.a >> 7) == 1;
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);

        cpu.poll_interrupt();
    }

    pub fn pha(cpu: &mut Cpu, data: modes::data) {
        let value = cpu.a;
        cpu.push(value);

        cpu.poll_interrupt();
    }

    pub fn php(cpu: &mut Cpu, data: modes::data) {
        let result = cpu.flags;
        cpu.push(result);

        cpu.poll_interrupt();
    }

    pub fn pla(cpu: &mut Cpu, _data: modes::data) {
        cpu.a = cpu.pop();

        cpu.poll_interrupt();
    }

    pub fn plp(cpu: &mut Cpu, data: modes::data) {
        cpu.flags = cpu.pop();

        cpu.poll_interrupt();
    }

    pub fn rol(cpu: &mut Cpu, data: modes::data) {
        let result = data.get(cpu).rotate_left(1);
        let c = (result & 1) == 1;

        let z = result == 0;
        let n = (result >> 7) == 0;
        cpu.set_flag(0, c);
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);
        data.set(cpu, result);

        cpu.poll_interrupt();
    }

    pub fn ror(cpu: &mut Cpu, data: modes::data) {
        let result = data.get(cpu).rotate_right(1);
        let c = (data.get(cpu) >> 7) == 1;

        let z = result == 0;
        let n = (result >> 7) == 0;
        cpu.set_flag(0, c);
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);
        data.set(cpu, result);

        cpu.poll_interrupt();
    }

    pub fn rti(cpu: &mut Cpu, _data: modes::data) {
        cpu.flags = cpu.pop();
        let lo = cpu.pop();
        let hi = cpu.pop();
        cpu.pc = ((hi as u16) << 8) + lo as u16;

        cpu.poll_interrupt();
    }

    pub fn rts(cpu: &mut Cpu, _data: modes::data) {
        let lo = cpu.pop();
        let hi = cpu.pop();
        cpu.pc = (((hi as u16) << 8) + lo as u16).wrapping_sub(1);

        cpu.poll_interrupt();
    }
    
    pub fn sbc(cpu: &mut Cpu, data: modes::data) {
        let result = (cpu.a as u16) + !(data.get(cpu) as u16) + (cpu.get_flag(0) as u16);

        let z = result == 0;
        let n = (result >> 7) == 1;
        let v = (!((((result % 256) as u8) >> 7) ^ (cpu.a >> 7)) & (cpu.a ^ cpu.get_flag(0) as u8))
            == 1;
        let c = result > 255;
        cpu.set_flag(1, z);
        cpu.set_flag(7, n);
        cpu.set_flag(6, v);
        cpu.set_flag(0, c);
        cpu.a = (result % 256) as u8;

        cpu.poll_interrupt();
    }

    pub fn sec(cpu: &mut Cpu, _data: modes::data) {
        cpu.set_flag(0, true);

        cpu.poll_interrupt();
    }
    
    pub fn sed(cpu: &mut Cpu, _data: modes::data) {
        cpu.set_flag(3, true);

        cpu.poll_interrupt();
    }


    pub fn sei(cpu: &mut Cpu, _data: modes::data) {
        cpu.set_flag(2, true);

        cpu.poll_interrupt();
    }

    pub fn sta(cpu: &mut Cpu, data: modes::data) {
        match data {
            modes::data::Memory(a) => {
                cpu.mem.set(a, cpu.a);

                cpu.poll_interrupt();
            }
            _ => unreachable!(),
        }
    }

    pub fn stx(cpu: &mut Cpu, data: modes::data) {
        match data {
            modes::data::Memory(a) => {
                cpu.mem.set(a, cpu.x);

                cpu.poll_interrupt();
            }
            _ => unreachable!(),
        }
    }

    pub fn sty(cpu: &mut Cpu, data: modes::data) {
        match data {
            modes::data::Memory(a) => {
                cpu.mem.set(a, cpu.y);

                cpu.poll_interrupt();
            }
            _ => unreachable!(),
        }
    }
    
    pub fn tax(cpu: &mut Cpu, _data: modes::data) {
        cpu.x = cpu.a;

        cpu.poll_interrupt();
    }
    
    pub fn tay(cpu: &mut Cpu, _data: modes::data) {
        cpu.y = cpu.a;

        cpu.poll_interrupt();
    }
    
    pub fn tsx(cpu: &mut Cpu, _data: modes::data) {
        cpu.x = cpu.s;

        cpu.poll_interrupt();
    }

    pub fn txa(cpu: &mut Cpu, _data: modes::data) {
        cpu.a = cpu.x;

        cpu.poll_interrupt();
    }

    pub fn txs(cpu: &mut Cpu, _data: modes::data) {
        cpu.s = cpu.x;

        cpu.poll_interrupt();
    }

    pub fn tya(cpu: &mut Cpu, _data: modes::data) {
        cpu.a = cpu.y;

        cpu.poll_interrupt();
    }

    fn branch(cpu: &mut Cpu, data: modes::data, flag: bool) {
        if flag {
            match data {
                modes::data::Memory(a) => cpu.pc = a,
                _ => {}
            }
        }
    }
}
