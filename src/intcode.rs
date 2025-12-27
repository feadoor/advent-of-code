pub struct IntcodeRunner {
    program: IntcodeMemory,
    pc: usize,
}

impl IntcodeRunner {
    pub fn for_program(program: Vec<usize>) -> Self {
        Self {
            program: IntcodeMemory(program),
            pc: 0,
        }
    }

    pub fn run(&mut self) {
        while *self.read(self.pc) != 99 { self.step(); }
    }

    pub fn read(&mut self, index: usize) -> &usize {
        self.program.get(index)
    }

    fn read_pc(&mut self) -> &usize {
        self.pc += 1;
        self.read(self.pc - 1)
    }

    fn write(&mut self, index: usize, value: usize) {
        *self.program.get_mut(index) = value;
    } 

    fn opcode1(&mut self) {
        let arg1 = *self.read_pc(); let arg2 = *self.read_pc(); let dst = *self.read_pc();
        let result = *self.read(arg1) + *self.read(arg2);
        self.write(dst, result);
    }

    fn opcode2(&mut self) {
        let arg1 = *self.read_pc(); let arg2 = *self.read_pc(); let dst = *self.read_pc();
        let result = *self.read(arg1) * *self.read(arg2);
        self.write(dst, result);
    }

    fn step(&mut self) {
        match self.read_pc() {
            1 => self.opcode1(),
            2 => self.opcode2(),
            99 => {},
            x => panic!("Unexpected opcode {}", x),
        }
    }
}

struct IntcodeMemory(Vec<usize>);

impl IntcodeMemory {
    fn get(&mut self, index: usize) -> &usize {
        self.extend_to(index);
        &self.0[index]
    }

    fn get_mut(&mut self, index: usize) -> &mut usize {
        self.extend_to(index);
        &mut self.0[index]
    }

    fn extend_to(&mut self, index: usize) {
        if self.0.len() <= index { self.0.resize(2 * index, 0); }
    }
}
