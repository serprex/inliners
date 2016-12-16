use std::mem;
use std::io::{self, BufRead, Read};

#[derive(Default)]
pub struct Machine {
	regs: [u8; 256],
	mem: Vec<u8>,
	prog: Vec<u8>,
	stack: Vec<usize>,
	temp: u64,
	pc: usize,
}

/*
0000 reg/reg
00000000 mov
00000001 swap
00000010 ++
00000011 --
00000100 **
00000101 /
00000110 ^^
00000111 &
00001000 |
00001001 ^
00001010 >>
00001011 <<
00001100 >>>
00001101 <<<
0000111X ???

0001 mixed move
00010000 reg2mem
00010001 mem2reg
00010010 swap
00010011 imm2reg
000101XX ???
000110XX imm2mem
000111XX zero2mem

10 condition
idea: use a bit to specify 2nd label
10000XXX <
10001XXX <=
10010XXX =
10011XXX >=
10100XXX >
10101XXX !
1011XXXX ???

01XXXXXX ???

111 special
111000 reg
11100000 print char from reg
11100001 print number from reg
11100010 incr reg
11100011 decr reg
111001 mem
11100100 print char from mem
11100101 print string from mem
11100110 read str to mem
11100111 read char to mem
111100 label
11110000 jmp
11110001 call
11110010 str to mem
11110011 print
111101XX ???
11101XXX ???
11111 special special
111110XX print number from mem
11111110 return
11111111 halt
*/

impl Machine {
	pub fn run(&mut self) {
		loop {
			let op = self.prog[self.pc];
			match op {
				x if (x&224) == 0 => {
					let r1 = self.prog[self.pc+1];
					let r2 = self.prog[self.pc+2];
					self.pc += 3;
					self.prog[self.pc - 2] = match op {
						0 => r2,
						1 => {
							self.prog[self.pc-1] = r1;
							r2
						},
						2 => r1.wrapping_add(r2),
						3 => r1.wrapping_sub(r2),
						4 => r1.wrapping_mul(r2),
						5 => {
							self.prog[self.pc-1] = r1 % r2;
							r1 / r2
						},
						6 => {
							let r = 1u8;
							for _ in 0..r2 {
								r = r.wrapping_mul(r1);
							}
							r
						},
						7 => r1 & r2,
						8 => r1 | r2,
						9 => r1 ^ r2,
						10 => r1.wrapping_shr(r2 as u32),
						11 => r1.wrapping_shl(r2 as u32),
						12 => r1.rotate_right(r2 as u32),
						13 => r1.rotate_left(r2 as u32),
						_ => panic!("Illegal 000"), 
					};
				},
				32 => {
					let rid = self.prog[self.pc+1] as usize;
					self.pc += 1;
					let mem = self.read_mem1();
					self.regs[rid] = mem;
				},
				33 => {
					let rid = self.prog[self.pc+1] as usize;
					self.pc += 1;
					let mut loc = self.get_mem_offset();
					self.mem[loc] = self.regs[rid];
				},
				34 => {
					let rid = self.prog[self.pc+1] as usize;
					self.pc += 1;
					let mut loc = self.get_mem_offset();
					let m = self.mem[loc];
					self.mem[loc] = self.regs[rid];
					self.regs[rid] = m;
				},
				35 => {
					self.regs[self.prog[self.pc + 1] as usize] = self.prog[self.pc + 2];
					self.pc += 2;
				}
				x if (x&252) == 48 => {
					let n = op&3;
					let loc = self.get_mem_offset();
					self.mem[loc] = self.prog[self.pc];
					self.pc += 1;
					if n > 0 {
						self.mem[loc+1] = self.prog[self.pc];
						self.pc += 1;
						if n > 1 {
							self.mem[loc+2] = self.prog[self.pc];
							self.pc += 1;
							if n > 2 {
								self.mem[loc+3] = self.prog[self.pc];
								self.pc += 1;
							}
						}
					}
				},
				x if (x&252) == 56 => {
					let n = op&3;
					let loc = self.get_mem_offset();
					self.mem[loc] = 0;
					if n > 0 {
						self.mem[loc+1] = 0;
						if n > 1 {
							self.mem[loc+2] = 0;
							if n > 2 {
								self.mem[loc+3] = 0;
							}
						}
					}
				},
				x if (x&192) == 128 => {
					let r = self.reg[self.prog[self.pc+1] as usize];
					let n = op&3;
					let b = match op&248 {
						128 => r < self.temp as u8,
						136 => r <= self.temp as u8,
						144 => r == self.temp as u8,
						152 => r >= self.temp as u8,
						160 => r > self.temp as u8,
						168 => r != self.temp as u8,
					};
					self.pc = if b {
						let mut x = self.prog[self.pc+2] as u32;
						if n > 0 {
							x = (x << 8) | self.prog[self.pc+3] as u32;
							if n > 1 {
								x = (x << 8) | self.prog[self.pc+4] as u32;
								if n > 2 {
									x = (x << 8) | self.prog[self.pc+5] as u32;
								}
							}
						}
						x as usize
					} else {
						n as usize + 3
					};
				},
				224 => {
					print!("{}", self.prog[self.pc+1] as char);
					self.pc += 2;
				},
				225 => {
					print!("{}", self.prog[self.pc+1]);
					self.pc += 2;
				},
				226 => {
					self.regs[self.prog[self.pc+1]] += 1;
					self.pc += 2;
				}
				227 => {
					self.regs[self.prog[self.pc+1]] -= 1;
					self.pc += 2;
				},
				228 => print!("{}", self.get_mem1() as char),
				229 => {
					let mut loc = self.get_mem_offset();
					loop {
						if self.mem[loc] == 0 {
							break
						}
						print!("{}", self.mem[loc] as char);
						loc += 1;
					}
				},
				230 => {
					let mut loc = self.get_mem_offset();
					loop {
						let stdin = io::stdin();
						let stdinLock = stdin.lock();
						let mut line = String::new();
						stdinLock.read_line(&mut line).ok();
						for b in line.bytes() {
							self.mem[loc] = b;
							loc += 1;
						}
					}
				},
				231 => {
					let mut loc = self.get_mem_offset();
					let stdin = io::stdin();
					if let Some(b) = stdin.bytes().next() {
						self.mem[loc] = b;
					}
				},
				x if (x&252) == 248 => {
					let n = (op&3)+1;
					let mut loc = self.get_mem_offset();
					let mut x = self.mem[loc] as u32;
					if n > 0 {
						x = (x << 8) | self.mem[loc+1] as u32;
						if n > 1 {
							x = (x << 8) | self.mem[loc+2] as u32;
							if n > 2 {
								x = (x << 8) | self.mem[loc+3] as u32;
							}
						}
					}
					print!("{}", x);
				},
				254 => self.pc = self.stack.pop().unwrap(),
				255 => return,
				_ => {
					println!("Illegal opcode {} @ {}", op, self.pc);
					return
				}
			}
		}
	}
}
