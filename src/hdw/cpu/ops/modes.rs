use hdw::cpu::Cpu;

pub fn accumulator(cpu: &mut Cpu) -> data {
    cpu.pc = cpu.pc.wrapping_add(1);
    data::A
}

pub fn absolute(cpu: &mut Cpu) -> data {
    let address = ((cpu.mem.get(cpu.pc.wrapping_add(2)) as u16) << 8)
        | (cpu.mem.get(cpu.pc.wrapping_add(1)) as u16);

    cpu.pc = cpu.pc.wrapping_add(3);
    data::Memory(address)
}

pub fn absolutex(cpu: &mut Cpu) -> data {
    let absolute = ((cpu.mem.get(cpu.pc.wrapping_add(2)) as u16) << 8)
        | (cpu.mem.get(cpu.pc.wrapping_add(1)) as u16);
    let result = absolute.wrapping_add(cpu.x.into());
    cpu.pc = cpu.pc.wrapping_add(3);
    data::Memory(result)
}

pub fn absolutey(cpu: &mut Cpu) -> data {
    let absolute = ((cpu.mem.get(cpu.pc.wrapping_add(2)) as u16) << 8)
        | (cpu.mem.get(cpu.pc.wrapping_add(1)) as u16);
    let result = absolute.wrapping_add(cpu.y.into());
    cpu.pc = cpu.pc.wrapping_add(3);
    data::Memory(result)
}

pub fn immidiate(cpu: &mut Cpu) -> data {
    let result = cpu.mem.get(cpu.pc.wrapping_add(1));
    cpu.pc = cpu.pc.wrapping_add(2);

    data::Value(result)
}

pub fn implied(cpu: &mut Cpu) -> data {
    cpu.pc = cpu.pc.wrapping_add(1);
    data::Implied
}

pub fn indirect(cpu: &mut Cpu) -> data {
    let ptr = ((cpu.mem.get(cpu.pc.wrapping_add(2)) as u16) << 8)
        + (cpu.mem.get(cpu.pc.wrapping_add(1)) as u16);
    let address = ((cpu.mem.get(ptr + 1) as u16) << 8) | (cpu.mem.get(ptr) as u16);

    cpu.pc = cpu.pc.wrapping_add(2);
    data::Memory(address)
}

pub fn xindirect(cpu: &mut Cpu) -> data {
    let arg = cpu.mem.get(cpu.pc.wrapping_add(1));
    let address = cpu.mem.get((arg + cpu.x).into()) as u16
        + ((cpu.mem.get((arg + cpu.x + 1).into()) as u16) << 8);

    cpu.pc = cpu.pc.wrapping_add(2);
    data::Memory(address)
}

pub fn indirecty(cpu: &mut Cpu) -> data {
    let arg = cpu.mem.get(cpu.pc.wrapping_add(1));
    let address =
        (cpu.mem.get(arg.into()) as u16) + ((cpu.mem.get((arg + 1).into()) as u16) << 8) + (cpu.y as u16);

    cpu.pc = cpu.pc.wrapping_add(2);
    data::Memory(address)
}

pub fn relative(cpu: &mut Cpu) -> data {
    let arg = cpu.mem.get(cpu.pc.wrapping_add(1));
    
    cpu.pc = cpu.pc.wrapping_add(2);

    let lo = (cpu.pc % 256) as u8;
    let hi = (cpu.pc >> 8) as u8;

    let result = ((hi as u16) << 8) + (lo.wrapping_add(arg) as u16);

    data::Memory(result)
}

pub fn zeropage(cpu: &mut Cpu) -> data {
    let arg = cpu.mem.get(cpu.pc.wrapping_add(1));

    cpu.pc = cpu.pc.wrapping_add(2);
    data::Memory(arg.into())
}

pub fn zeropagex(cpu: &mut Cpu) -> data {
    let arg = cpu.mem.get(cpu.pc.wrapping_add(1));
    let result = arg.wrapping_add(cpu.x.into());

    cpu.pc = cpu.pc.wrapping_add(2);
    data::Memory(result.into())
}

pub fn zeropagey(cpu: &mut Cpu) -> data {
    let arg = cpu.mem.get(cpu.pc.wrapping_add(1));
    let result = arg.wrapping_add(cpu.y.into());

    cpu.pc = cpu.pc.wrapping_add(2);
    data::Memory(result.into())
}

pub enum data {
    A,
    Memory(u16),
    Value(u8),
    Implied,
}

impl data {
    pub fn get(&self, cpu: &mut Cpu) -> u8 {
        match self {
            &data::A => cpu.a,
            &data::Memory(address) => cpu.mem.get(address.clone()),
            &data::Value(val) => val.clone(),
            &data::Implied => 0,
        }
    }
    pub fn set(&self, cpu: &mut Cpu, val: u8) {
        match self {
            &data::A => cpu.a = val,
            &data::Memory(address) => cpu.mem.set(address.clone(), val),
            &data::Value(val) => unreachable!(),
            &data::Implied => unreachable!(),
        }
    }
}
