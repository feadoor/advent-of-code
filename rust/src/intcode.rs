use itertools::iterate;
use std::collections::VecDeque;

pub struct IntcodeRunner {
    program: IntcodeMemory,
    pc: usize,
    relative_base: isize,
    inputs: VecDeque<isize>,
    pub outputs: VecDeque<isize>,
}

pub enum IntcodeInterrupt {
    NeedsInput,
    Halt,
}

impl IntcodeRunner {
    pub fn for_program(program: Vec<isize>) -> Self {
        Self {
            program: IntcodeMemory(program),
            pc: 0,
            relative_base: 0,
            inputs: VecDeque::new(),
            outputs: VecDeque::new(),
        }
    }

    pub fn run(&mut self) -> IntcodeInterrupt {
        loop { 
            if let Some(interrupt) = self.step() {
                return interrupt;
            }
        }
    }

    pub fn get(&mut self, index: usize) -> isize {
        *self.program.get(index)
    }

    pub fn push_input(&mut self, input: isize) {
        self.inputs.push_back(input);
    }

    pub fn push_inputs<I: IntoIterator<Item = isize>>(&mut self, inputs: I) {
        self.inputs.extend(inputs.into_iter())
    }

    pub fn pop_output(&mut self) -> Option<isize> {
        self.outputs.pop_front()
    }

    pub fn last_output(&self) -> Option<isize> {
        self.outputs.back().copied()
    }

    fn read(&mut self) -> isize {
        self.pc += 1;
        self.get(self.pc - 1)
    }

    fn write(&mut self, index: usize, value: isize) {
        *self.program.get_mut(index) = value;
    }

    fn load_opcode(&mut self) -> (isize, impl Iterator<Item = isize> + use<>) {
        let raw_opcode = self.read();
        let (opcode, modes) = (raw_opcode % 100, raw_opcode / 100);
        (opcode, iterate(modes, |&x| x / 10).take_while(|&x| x > 0).map(|x| x % 10))
    }

    fn load_param<I: Iterator<Item = isize>>(&mut self, modes: &mut I) -> isize {
        let mode = modes.next().unwrap_or(0);
        match mode {
            0 => self.load_position(),
            1 => self.load_immediate(),
            2 => self.load_relative(),
            _ => panic!("Unknown mode {}", mode),
        }
    }

    fn load_dst<I: Iterator<Item = isize>>(&mut self, modes: &mut I) -> usize {
        let mode = modes.next().unwrap_or(0);
        let addr = match mode {
            0 => self.read(),
            2 => self.read() + self.relative_base,
            _ => panic!("Unexpected mode for destination param: {}", mode),
        };
        assert!(addr >= 0); addr as usize
    }

    fn load_position(&mut self) -> isize {
        let addr = self.read(); assert!(addr >= 0);
        self.get(addr as usize)
    }

    fn load_immediate(&mut self) -> isize {
        self.read()
    }

    fn load_relative(&mut self) -> isize {
        let addr = self.read() + self.relative_base; assert!(addr >= 0);
        self.get(addr as usize)
    }

    fn opcode1<I: Iterator<Item = isize>>(&mut self, modes: &mut I) -> Option<IntcodeInterrupt> {
        let arg1 = self.load_param(modes); let arg2 = self.load_param(modes); let dst = self.load_dst(modes);
        self.write(dst, arg1 + arg2);
        None
    }

    fn opcode2<I: Iterator<Item = isize>>(&mut self, modes: &mut I) -> Option<IntcodeInterrupt> {
        let arg1 = self.load_param(modes); let arg2 = self.load_param(modes); let dst = self.load_dst(modes);
        self.write(dst, arg1 * arg2);
        None
    }

    fn opcode3<I: Iterator<Item = isize>>(&mut self, modes: &mut I) -> Option<IntcodeInterrupt> {
        if let Some(input) = self.inputs.pop_front() {
            let dst = self.load_dst(modes);
            self.write(dst, input);
            None
        } else {
            self.pc -= 1;
            Some(IntcodeInterrupt::NeedsInput)
        }
    }

    fn opcode4<I: Iterator<Item = isize>>(&mut self, modes: &mut I) -> Option<IntcodeInterrupt> {
        let arg1 = self.load_param(modes);
        self.outputs.push_back(arg1);
        None
    }

    fn opcode5<I: Iterator<Item = isize>>(&mut self, modes: &mut I) -> Option<IntcodeInterrupt> {
        let arg1 = self.load_param(modes); let arg2 = self.load_param(modes);
        if arg1 != 0 { assert!(arg2 >= 0); self.pc = arg2 as usize; }
        None
    }

    fn opcode6<I: Iterator<Item = isize>>(&mut self, modes: &mut I) -> Option<IntcodeInterrupt> {
        let arg1 = self.load_param(modes); let arg2 = self.load_param(modes);
        if arg1 == 0 { assert!(arg2 >= 0); self.pc = arg2 as usize; }
        None
    }

    fn opcode7<I: Iterator<Item = isize>>(&mut self, modes: &mut I) -> Option<IntcodeInterrupt> {
        let arg1 = self.load_param(modes); let arg2 = self.load_param(modes); let dst = self.load_dst(modes);
        self.write(dst, if arg1 < arg2 { 1 } else { 0 });
        None
    }

    fn opcode8<I: Iterator<Item = isize>>(&mut self, modes: &mut I) -> Option<IntcodeInterrupt> {
        let arg1 = self.load_param(modes); let arg2 = self.load_param(modes); let dst = self.load_dst(modes);
        self.write(dst, if arg1 == arg2 { 1 } else { 0 });
        None
    }

    fn opcode9<I: Iterator<Item = isize>>(&mut self, modes: &mut I) -> Option<IntcodeInterrupt> {
        let arg1 = self.load_param(modes);
        self.relative_base += arg1;
        None
    }

    fn opcode99(&mut self) -> Option<IntcodeInterrupt> {
        self.pc -= 1;
        Some(IntcodeInterrupt::Halt)
    }

    fn step(&mut self) -> Option<IntcodeInterrupt> {
        let (opcode, mut modes) = self.load_opcode();
        match opcode {
            1 => self.opcode1(&mut modes),
            2 => self.opcode2(&mut modes),
            3 => self.opcode3(&mut modes),
            4 => self.opcode4(&mut modes),
            5 => self.opcode5(&mut modes),
            6 => self.opcode6(&mut modes),
            7 => self.opcode7(&mut modes),
            8 => self.opcode8(&mut modes),
            9 => self.opcode9(&mut modes),
            99 => self.opcode99(),
            x => panic!("Unexpected opcode {}", x),
        }
    }
}

struct IntcodeMemory(Vec<isize>);

impl IntcodeMemory {
    fn get(&mut self, index: usize) -> &isize {
        self.extend_to(index);
        &self.0[index]
    }

    fn get_mut(&mut self, index: usize) -> &mut isize {
        self.extend_to(index);
        &mut self.0[index]
    }

    fn extend_to(&mut self, index: usize) {
        if self.0.len() <= index { self.0.resize(2 * index, 0); }
    }
}
