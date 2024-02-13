use crate::hdw::cpu::Cpu;

pub fn accumulator(cpu: &mut Cpu) -> Data {
    cpu.pc = cpu.pc.wrapping_add(1);
    Data::A
}

pub fn absolute(cpu: &mut Cpu) -> Data {
    let address = ((cpu.mem.get(cpu.pc.wrapping_add(2)) as u16) << 8)
        | (cpu.mem.get(cpu.pc.wrapping_add(1)) as u16);

    cpu.pc = cpu.pc.wrapping_add(3);
    Data::Memory(address)
}

pub fn absolutex(cpu: &mut Cpu) -> Data {
    let absolute = ((cpu.mem.get(cpu.pc.wrapping_add(2)) as u16) << 8)
        | (cpu.mem.get(cpu.pc.wrapping_add(1)) as u16);
    let result = absolute.wrapping_add(cpu.x.into());
    cpu.pc = cpu.pc.wrapping_add(3);
    Data::Memory(result)
}

pub fn absolutey(cpu: &mut Cpu) -> Data {
    let absolute = ((cpu.mem.get(cpu.pc.wrapping_add(2)) as u16) << 8)
        | (cpu.mem.get(cpu.pc.wrapping_add(1)) as u16);
    let result = absolute.wrapping_add(cpu.y.into());
    cpu.pc = cpu.pc.wrapping_add(3);
    Data::Memory(result)
}

pub fn immediate(cpu: &mut Cpu) -> Data {
    let result = cpu.mem.get(cpu.pc.wrapping_add(1));
    cpu.pc = cpu.pc.wrapping_add(2);

    Data::Value(result)
}

pub fn implied(cpu: &mut Cpu) -> Data {
    cpu.pc = cpu.pc.wrapping_add(1);
    Data::Implied
}

pub fn indirect(cpu: &mut Cpu) -> Data {
    let ptr_high = (cpu.mem.get(cpu.pc.wrapping_add(2)) as u16) << 8;
    let ptr_low = cpu.mem.get(cpu.pc.wrapping_add(1));
    let address = ((cpu.mem.get(ptr_high + ptr_low.wrapping_add(1) as u16) as u16) << 8)
        | (cpu.mem.get(ptr_high + ptr_low as u16) as u16);

    cpu.pc = cpu.pc.wrapping_add(2);
    Data::Memory(address)
}

pub fn xindirect(cpu: &mut Cpu) -> Data {
    let arg = cpu.mem.get(cpu.pc.wrapping_add(1));
    let address = cpu.mem.get((arg + cpu.x).into()) as u16
        + ((cpu.mem.get((arg + cpu.x + 1).into()) as u16) << 8);

    cpu.pc = cpu.pc.wrapping_add(2);
    Data::Memory(address)
}

pub fn indirecty(cpu: &mut Cpu) -> Data {
    let arg = cpu.mem.get(cpu.pc.wrapping_add(1));
    let address = (cpu.mem.get(arg.into()) as u16)
        + ((cpu.mem.get((arg + 1).into()) as u16) << 8)
        + (cpu.y as u16);

    cpu.pc = cpu.pc.wrapping_add(2);
    Data::Memory(address)
}

pub fn relative(cpu: &mut Cpu) -> Data {
    let mut arg = cpu.mem.get(cpu.pc.wrapping_add(1)) as i16;

    if arg > 127 {
        arg -= 256;
    }

    cpu.pc = cpu.pc.wrapping_add(2);

    let result = cpu.pc.wrapping_add_signed(arg);

    Data::Memory(result)
}

pub fn zeropage(cpu: &mut Cpu) -> Data {
    let arg = cpu.mem.get(cpu.pc.wrapping_add(1));

    cpu.pc = cpu.pc.wrapping_add(2);
    Data::Memory(arg.into())
}

pub fn zeropagex(cpu: &mut Cpu) -> Data {
    let arg = cpu.mem.get(cpu.pc.wrapping_add(1));
    let result = arg.wrapping_add(cpu.x.into());

    cpu.pc = cpu.pc.wrapping_add(2);
    Data::Memory(result.into())
}

pub fn zeropagey(cpu: &mut Cpu) -> Data {
    let arg = cpu.mem.get(cpu.pc.wrapping_add(1));
    let result = arg.wrapping_add(cpu.y.into());

    cpu.pc = cpu.pc.wrapping_add(2);
    Data::Memory(result.into())
}

pub enum Data {
    A,
    Memory(u16),
    Value(u8),
    Implied,
}

impl Data {
    pub fn get(&self, cpu: &mut Cpu) -> u8 {
        match self {
            &Data::A => cpu.a,
            &Data::Memory(address) => cpu.mem.get(address.clone()),
            &Data::Value(val) => val.clone(),
            &Data::Implied => 0,
        }
    }
    pub fn set(&self, cpu: &mut Cpu, val: u8) {
        match self {
            &Data::A => cpu.a = val,
            &Data::Memory(address) => cpu.mem.set(address.clone(), val),
            &Data::Value(val) => unreachable!(),
            &Data::Implied => unreachable!(),
        }
    }
}
