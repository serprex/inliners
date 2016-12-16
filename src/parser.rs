use std::io::{self, stdin, Read, BufRead};
use std::mem;
use std::str::Bytes;
use fnv::FnvHashMap;
use regex::bytes::{RegexSet, Regex};

#[derive(Debug)]
pub enum Instr {
	Reg2Reg(u8, u8),
	Reg2Mem(u8),
	Mem2Reg(u8),
	Reg2MR(u8),
	Imm2MR(u8),
	SwapMem(u8),
	SwapReg(u8, u8),
	Imm2Reg(u8, u8),
	Imm2Mem(u8),
	Str2Mem(Vec<u8>),
	CmpC(CmpOp, u8, Vec<u8>),
	CmpR(CmpOp, u8, Vec<u8>),
	LoopC(CmpOp, u8, u8, Vec<u8>),
	LoopR(CmpOp, u8, u8, Vec<u8>),
	PCReg(u8),
	PNReg(u8),
	Incr(u8),
	Decr(u8),
	PCMem,
	PNMem,
	PSMem,
	Print(Vec<u8>),
	Call(Vec<u8>),
	Jmp(Vec<u8>),
	GCMem,
	GSMem,
	EndLine,
	Return,
	Halt,
}

#[derive(Copy, Clone, Debug)]
enum CmpOp {
	LT,
	LE,
	EQ,
	GE,
	GT,
	NE,
}

fn parse_a(s: &[u8]) -> u8 {
	if let Some(&c) = s.iter().next() {
		if c == b'\\' {
			if s.len() > 2 {
				panic!("Too long :a (2)");
			}
			match s.iter().cloned().nth(2).unwrap() {
				c2 @ b'0' ... b'9' => c2 - b'0',
				b't' => b'\t',
				b'r' => b'\r',
				b'n' => b'\n',
				c2 => c2,
			}
		} else if s.len() > 1 {
			panic!("Too long :a (1)");
		} else {
			c
		}
	} else {
		panic!("Empty :a");
	}
}
fn parse_h(s: &[u8]) -> u8 {
	let mut m: usize = 1;
	let mut ret: usize = 0;
	if s.len() > 2 {
		panic!("Too long :h");
	}
	for &c in s.iter().rev() {
		ret += m * match c {
			b'0' ... b'9' => (c - b'0') as usize,
			b'a' ... b'f' => (10 + (c - b'a')) as usize,
			b'A' ... b'F' => (10 + (c - b'A')) as usize,
			_ => panic!("Invalid :h"),
		};
		m *= 10;
	}
	ret as u8
}
fn parse_d(s: &[u8]) -> u8 {
	let mut m: usize = 1;
	let mut ret: usize = 0;
	if s.len() > 3 {
		panic!("Too long :d");
	}
	for &c in s.iter().rev() {
		match c {
			b'0' ... b'9' => ret += m * (c - b'0') as usize,
			_ => panic!("Invalid :d"),
		}
		m *= 10;
	}
	if ret > 255 {
		panic!("Too big :d")
	}
	ret as u8
}
fn parse_b(s: &[u8]) -> u8 {
	let mut m = 1;
	let mut ret = 0;
	if s.len() > 8 {
		panic!("Too long :b");
	}
	for &c in s.iter().rev() {
		if c == b'1' {
			ret += m;
		} else if c != b'0' {
			panic!("Invalid :b");
		}
		m <<= 1;
	}
	ret
}

fn parse_reg(chs: &[u8]) -> (&[u8], u8) {
	let mut idx = 0;
	let mut indice = Vec::new();
	loop {
		match chs[idx] {
			b':' => {
				let ret = match chs[idx+1] {
					b'a' => parse_a(&indice),
					b'b' => parse_b(&indice),
					b'd' => parse_d(&indice),
					b'h' => parse_h(&indice),
					_ => panic!("Unknown numerical mode in deref"),
				};
				return (&chs[idx+2..], ret)
			},
			x => indice.push(x),
		}
		idx += 1;
	}
}

fn parse_imm(chs: &[u8]) -> (&[u8], u8) {
	let mut idx = 0;
	while chs[idx] != b'(' { idx += 1 }
	let (chs, val) = parse_reg(&chs[idx+1..]);
	if chs[0] != b')' {
		panic!("Expected )");
	}
	(&chs[1..], val)
}

fn parse_str(chs: &[u8]) -> (&[u8], Vec<u8>) {
	let mut idx = 0;
	let mut indice = Vec::new();
	while chs[idx] != b'(' { idx += 1 }
	idx += 1;
	while chs[idx] != b':' && chs[idx+1] != b'a' && chs[idx+2] != b')' {
		indice.push(chs[idx]);
		idx += 1;
	}
	(&chs[idx+3..], indice)
}

fn parse_mem<'a, 'b>(chs: &'a [u8], parser: &'b mut Parser) -> &'a [u8] {
	let mut idx = 0;
	let mut indices = Vec::new();
	let mut indice = Vec::new();
	while chs[idx] != b'[' { idx += 1 }
	idx += 1;
	loop {
		match chs[idx] {
			b'-' => {
				let ind = mem::replace(&mut indice, Vec::new());
				indices.push(ind);
			},
			b':' => {
				let ind = mem::replace(&mut indice, Vec::new());
				indices.push(ind);
				idx += 1;
				let indvals = match chs[idx] {
					b'a' => indices.iter().map(|x| parse_a(x)).collect::<Vec<u8>>(),
					b'b' => indices.iter().map(|x| parse_b(x)).collect::<Vec<u8>>(),
					b'd' => indices.iter().map(|x| parse_d(x)).collect::<Vec<u8>>(),
					b'h' => indices.iter().map(|x| parse_h(x)).collect::<Vec<u8>>(),
					_ => panic!("Unknown numerical mode in deref"),
				};
				let mut regref = false;
				idx += 1;
				match chs[idx] {
					b':' => {
						// TODO m, nesting
						idx += 1;
						match chs[idx] {
							b'r' => {
								regref = true;
								idx += 1;
								if chs[idx] != b']' {
									panic!("Expected ]")
								}
							},
							_ => panic!("Unexpected deref mode"),
						}
					}
					b']' => (),
					_ => panic!("Expected : or ]"),
				}
				for ind in indvals {
					if regref {
						parser.prog.push(Instr::Reg2MR(ind));
					} else {
						parser.prog.push(Instr::Imm2MR(ind));
					}
				}
				return &chs[idx+1..]
			},
			x => indice.push(x),
		}
		idx += 1;
	}
}

fn parse_lab(chs: &[u8]) -> (&[u8], Vec<u8>) {
	let mut idx = 0;
	while chs[idx] != b'{' { idx += 1 }
	idx += 1;
	let mut label = Vec::new();
	while chs[idx] != b'}' {
		label.push(chs[idx]);
		idx += 1;
	}
	(&chs[idx+1..], label)
}

#[derive(Debug)]
pub struct Parser {
	pub prog: Vec<Instr>,
	pub labels: FnvHashMap<Vec<u8>, usize>,
	pub labelstr: FnvHashMap<Vec<u8>, Vec<u8>>,
	regset: RegexSet,
}

macro_rules! rereg {
	() => { r"(.:a|\\.:a|[01]{8}:b|\d{1,3}:d|[0-9a-fA-F]{1,2}:h)" }
}
macro_rules! reconst {
	() => { concat!(r"\(", rereg!(), r"\)") }
}
macro_rules! remem {
	() => { r"(\[((.|\\.)(-(.|\\.))*:a|[01]{8}(-[01]{8})*:b|\d{1,3}(-\d{1,3})*:d|[0-9a-fA-F]{1,2}(-[0-9a-fA-F])*:h)(:r)?\])" }
}
macro_rules! relab {
	() => { r"\{[a-zA-Z0-9_]+\}" }
}
macro_rules! recond {
	() => { concat!(r"\((>|<|>=|<=|!|=)\s*", rereg!(), r"\)") }
}
macro_rules! recondreg {
	() => { concat!(r"\((>|<|>=|<=|!|=)\s*", rereg!(), r":r\)") }
}

const RULES: &'static [&'static str] = &[
	concat!(r"!s?\s*", remem!()),
	concat!(r"\*s\s*", remem!()),
	concat!(r"\*\s*", remem!()),
	concat!(r"\*\s*", rereg!()),
	concat!(r#""\s*"#, remem!()),
	concat!(r#""\s*"#, rereg!()),
	concat!(rereg!(), r"\s*>\s*", remem!()),
	concat!(rereg!(), r"\s*<\s*", remem!()),
	concat!(rereg!(), r"\s*<>\s*", remem!()),
	concat!(r"@\s*", relab!()),
	concat!(r"#\s*", relab!()),
	concat!(r"\?\s*", rereg!(), r"\s*", recond!(), r"\s*_\s*", relab!(), r"\s*_\s*\(\s*", relab!(), r"\)"),
	concat!(r"\?\s*", rereg!(), r"\s*", recondreg!(), r"\s*_\s*", relab!(), r"\s*_\s*\(\s*", relab!(), r"\)"),
	concat!(r"\?\s*", rereg!(), r"\s*", recond!(), r"\s*_\s*", relab!()),
	concat!(r"\?\s*", rereg!(), r"\s*", recondreg!(), r"\s*_\s*", relab!()),
	concat!(r"%\s*", relab!(), r"\s*", recond!(), r"\s*", reconst!()),
	concat!(r"%\s*", relab!(), r"\s*", recondreg!(), r"\s*", reconst!()),
	concat!(r"\\s*", rereg!(), r"\s*", reconst!()),
	concat!(r"\$\s*", remem!(), reconst!()),
	concat!(r"\$s\s*", remem!(), r"\s*\(.*:a\)"),
	concat!(r".\s*", rereg!(), r"\s*", rereg!()),
	concat!(r",\s*", rereg!(), r"\s*", rereg!()),
	concat!(r"\+\s*", rereg!()),
	concat!(r"-\s*", rereg!()),
	concat!(r"\+\+\s*", rereg!(), r"(\s*/\s*", rereg!(), r")+"),
	concat!(r"--\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r"\*\*\s*", rereg!(), r"(\s*/\s*", rereg!(), r")+"),
	concat!(r"\\\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r"\^\^\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r"<<\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r"<<<\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r">>\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r">>>\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r"&\s*", rereg!(), r"(\s*/\s*", rereg!(), r")+"),
	concat!(r"\|\s*", rereg!(), r"(\s*/\s*", rereg!(), r")+"),
	concat!(r"\^\s*", rereg!(), r"(\s*/\s*", rereg!(), r")+"),
	concat!(r"~\s*", relab!()),
	concat!(r"//"),
	concat!(r"/"),
	concat!(r";[^']*"),
	concat!(r"'.*"),
];

impl Default for Parser {
	fn default() -> Parser {
		Parser {
			prog: Vec::new(),
			labels: FnvHashMap::default(),
			labelstr: FnvHashMap::default(),
			regset: RegexSet::new(RULES).unwrap(),
		}
	}
}

fn skipws(s: &[u8]) -> &[u8] {
	let mut idx = 0;
	while {
		if idx == s.len() { return &[] }
		s[idx] == b' ' || s[idx] == b'\t'
	} {
		idx += 1;
	}
	&s[idx..]
}

impl Parser {
	pub fn parse_line(&mut self, line: &str) {
		if line.is_empty() { return }
		let mut line = line.as_bytes();
		let cur_label = if line[0] == b'{' {
			let mut idx = 0;
			let mut lab = Vec::new();
			loop {
				idx += 1;
				let c = line[idx];
				match c {
					b'a'...b'z'|b'A'...b'Z'|b'0'...b'9'|b'_' => lab.push(c),
					b'}' => break,
					_ => panic!("Unterminated label"),
				}
			}
			let pglen = self.prog.len();
			if self.labels.insert(lab.clone(), pglen).is_some() {
				println!("Duplicate label");
			}
			line = &line[idx+1..];
			lab
		} else {
			Vec::new()
		};
		println!("{:?}", line);
		while let Some(mid) = {
			line = skipws(line);
			self.regset.matches(line).into_iter().min()
		} {
			println!("{}\t{:?}", mid, line);
			match mid {
				0 => { // !s mem, s mem
					let isstr = line[1] == b's';
					line = skipws(&line[if isstr { 2 } else { 1 }..]);
					line = parse_mem(line, self);
					self.prog.push(if isstr { Instr::GSMem } else { Instr::GCMem });
				},
				1 => { // *s mem
					line = skipws(&line[2..]);
					line = parse_mem(line, self);
					self.prog.push(Instr::PSMem);
				},
				2 => { // * mem
					line = skipws(&line[1..]);
					line = parse_mem(line, self);
					self.prog.push(Instr::PCMem);
				},
				3 => { // * reg
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::PCReg(r));
				},
				4 => { // " mem
					line = skipws(&line[1..]);
					line = parse_mem(line, self);
					self.prog.push(Instr::PNMem);
				},
				5 => { // " reg
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::PNReg(r));
				},
				6 => { // reg > mem 
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					line = parse_mem(line, self);
					self.prog.push(Instr::Reg2Mem(r));
				},
				7 => { // reg < mem
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					line = parse_mem(line, self);
					self.prog.push(Instr::Mem2Reg(r));
				},
				8 => { // reg <> mem
					line = skipws(&line[2..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					line = parse_mem(line, self);
					self.prog.push(Instr::SwapMem(r));
				},
				9 => { // @ lab
					line = skipws(&line[1..]);
					let (newline, lab) = parse_lab(line);
					line = newline;
					self.prog.push(Instr::Jmp(lab));
					return
				},
				10 => { // # lab
					line = skipws(&line[1..]);
					let (newline, lab) = parse_lab(line);
					line = newline;
					self.prog.push(Instr::Call(lab));
				},
				11 => { // ? reg constcond _ lab
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
				},
				12 => { // ? reg regcond _ lab
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
				},
				13 => { // ? reg constcond _ lab _ (lab)
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
				},
				14 => { // ? reg regcond _ lab _ (lab)
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
				},
				15 => { // % lab constcond const
					line = skipws(&line[1..]);
					let (newline, lab) = parse_lab(line);
					line = skipws(newline);
				},
				16 => { // % lab regcond const
					line = skipws(&line[1..]);
					let (newline, lab) = parse_lab(line);
					line = skipws(newline);
				},
				17 => { // \ reg const
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					let (newline, c) = parse_imm(line);
					line = newline;
					self.prog.push(Instr::Imm2Reg(c, r));
				},
				18 => { // $ mem const
					line = skipws(&line[1..]);
					line = parse_mem(line, self);
					line = skipws(line);
					let (newline, c) = parse_imm(line);
					line = newline;
					self.prog.push(Instr::Imm2Mem(c));
				},
				19 => { // $s mem string
					line = skipws(&line[1..]);
					line = parse_mem(line, self);
					line = skipws(line);
					let (newline, cs) = parse_str(line);
					line = newline;
					self.prog.push(Instr::Str2Mem(cs));
				},
				20 => { // . reg reg
					line = skipws(&line[1..]);
					let (newline, r1) = parse_reg(line);
					line = skipws(newline);
					let (newline, r2) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::Reg2Reg(r2, r1));
				},
				21 => { // , reg reg
					line = skipws(&line[1..]);
					let (newline, r1) = parse_reg(line);
					line = skipws(newline);
					let (newline, r2) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::SwapReg(r1, r2));
				},
				22 => { // + reg
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::Incr(r));
				},
				23 => { // - reg
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::Decr(r));
				},
				24 => { // ++ reg reg..
					line = skipws(&line[2..]);
				},
				25 => { // -- reg reg
					line = skipws(&line[2..]);
				},
				26 => { // ** reg reg..
					line = skipws(&line[2..]);
				},
				27 => { // \ reg reg
					line = skipws(&line[1..]);
				},
				28 => { // ^^ reg reg
					line = skipws(&line[2..]);
				},
				29 => { // << reg reg
					line = skipws(&line[2..]);
				},
				30 => { // <<< reg reg
					line = skipws(&line[3..]);
				},
				31 => { // >> reg reg
					line = skipws(&line[2..]);
				},
				32 => { // >>> reg reg
					line = skipws(&line[3..]);
				},
				33 => { // & reg reg..
					line = skipws(&line[1..]);
				},
				34 => { // | reg reg..
					line = skipws(&line[1..]);
				},
				35 => { // ^ reg reg..
					line = skipws(&line[1..]);
				},
				36 => { // ~ lab
					line = skipws(&line[1..]);
					let (newline, lab) = parse_lab(line);
					line = newline;
					self.prog.push(Instr::Print(lab));
				},
				37 => { // //
					line = &line[2..];
					self.prog.push(Instr::Halt);
				},
				38 => { // /
					line = &line[1..];
					self.prog.push(Instr::Return);
				},
				39 => { // ;
					line = &line[1..];
					let mut ls = Vec::new();
					while !line.is_empty() && line[0] != b'\'' {
						ls.push(line[0]);
						line = &line[1..];
					}
					self.labelstr.insert(cur_label.clone(), ls);
					break
				},
				40 => { // '
					break
				},
				_ => unreachable!(),
			}
		}
		println!("END {:?}", line);
		if !cur_label.is_empty() {
			self.prog.push(Instr::EndLine);
		}
	}

	pub fn run(&self, mem: &mut [u8]) {
		let mut regs = [0u8; 256];
		let mut mptr = 0;
		let mut loopstack = Vec::new();
		let mut callstack = Vec::new();
		self.runcore(0, &mut regs, mem, &mut mptr, &mut loopstack, &mut callstack)
	}

	fn runcore(&self, mut pc: usize, regs: &mut [u8; 256], mem: &mut [u8], mptr: &mut usize, loopstack: &mut Vec<usize>, callstack: &mut Vec<usize>) {
		loop {
			match self.prog[pc] {
				Instr::Reg2Reg(r1, r2) => regs[r1 as usize] = regs[r2 as usize],
				Instr::Reg2Mem(r) => {
					mem[*mptr] = regs[r as usize];
					*mptr = 0;
				},
				Instr::Mem2Reg(r) => {
					regs[r as usize] = mem[*mptr];
					*mptr = 0;
				},
				Instr::Reg2MR(r) => {
					*mptr <<= 8;
					*mptr |= regs[r as usize] as usize;
				},
				Instr::Imm2MR(v) => {
					*mptr <<= 8;
					*mptr |= v as usize;
				},
				Instr::SwapMem(r) => {
					let t = regs[r as usize];
					regs[r as usize] = mem[*mptr];
					mem[*mptr] = t;
				},
				Instr::SwapReg(r1, r2) => {
					regs.swap(r1 as usize, r2 as usize);
				},
				Instr::Imm2Reg(r, v) => regs[r as usize] = v,
				Instr::Imm2Mem(v) => {
					mem[*mptr] = v;
					*mptr = 0;
				},
				Instr::Str2Mem(ref vs) => {
					for &v in vs.iter() {
						mem[*mptr] = v;
						*mptr += 1;
					}
					*mptr = 0;
				},
				Instr::PCReg(r) => {
					print!("{}", r as char);
				},
				Instr::PNReg(r) => {
					print!("{}", r);
				},
				Instr::Incr(r) => regs[r as usize] = regs[r as usize].wrapping_add(1),
				Instr::Decr(r) => regs[r as usize] = regs[r as usize].wrapping_sub(1),
				Instr::PCMem => {
					print!("{}", mem[*mptr] as char);
					*mptr = 0;
				},
				Instr::PNMem => {
					print!("{}", mem[*mptr]);
					*mptr = 0;
				},
				Instr::PSMem => {
					let mut out = String::new();
					while mem[*mptr] != 0 {
						out.push(mem[*mptr] as char);
						*mptr += 1;
					}
					print!("{}", out);
					*mptr = 0;
				},
				Instr::Print(ref lab) => {
					if let Some(chs) = self.labelstr.get(lab) {
						let mut out = String::new();
						for &ch in chs.iter() {
							out.push(ch as char);
							*mptr += 1;
						}
						print!("{}", out);
					}
					*mptr = 0;
				},
				Instr::Call(ref lab) => {
					if let Some(&loc) = self.labels.get(lab) {
						callstack.push(pc);
						pc = loc;
						continue
					} else {
						println!("Undefined label: {:?}", lab);
					}
				},
				Instr::Jmp(ref lab) => {
					if let Some(&loc) = self.labels.get(lab) {
						pc = loc;
						continue
					} else {
						println!("Undefined label: {:?}", lab);
					}
				},
				Instr::GCMem => {
					mem[*mptr] = stdin().bytes().next().unwrap_or(Ok(0)).unwrap();
					*mptr = 0;
				},
				Instr::GSMem => {
					let mut out = String::new();
					let sin = stdin();
					if let Ok(_) = sin.read_line(&mut out) {
						for b in out.bytes() {
							mem[*mptr] = b;
							*mptr += 1;
						}
					}
					*mptr = 0;
				},
				Instr::CmpC(_, _, _) => (),
				Instr::CmpR(_, _, _) => (),
				Instr::LoopC(_, _, _, _) => (),
				Instr::LoopR(_, _, _, _) => (),
				Instr::EndLine => {

				},
				Instr::Return => {
					if let Some(loc) = callstack.pop() {
						pc = loc;
					} else {
						println!("Callstack underflow");
					}
				},
				Instr::Halt => return,
			}
			pc += 1;
		}
	}
}