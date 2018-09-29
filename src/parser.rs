use fnv::FnvHashMap;
use regex::bytes::{Regex, RegexSet};
use std::io::{stdin, Read};
use std::mem;
use std::ops::Not;

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
	Cmp(Cop, u8, Vec<u8>),
	Loop(Cop, u8, u8, Vec<u8>),
	PCReg(u8),
	PNReg(u8),
	Incr(u8),
	Decr(u8),
	Add(Vec<u8>),
	Sub(u8, u8),
	Mul(Vec<u8>),
	DivMod(u8, u8),
	Exp(u8, u8),
	And(Vec<u8>),
	Or(Vec<u8>),
	Xor(Vec<u8>),
	Shl(u8, u8),
	Shr(u8, u8),
	Rol(u8, u8),
	Ror(u8, u8),
	PCMem,
	PNMem,
	PSMem,
	Print(Vec<u8>),
	Call(Vec<u8>),
	Jmp(Vec<u8>),
	GCMem,
	GSMem,
	Return,
	Halt,
}

#[derive(Copy, Clone)]
enum Cmp {
	LT,
	LE,
	EQ,
	GE,
	GT,
	NE,
}

#[derive(Copy, Clone, Debug)]
pub enum Cop {
	LTC(u8),
	LEC(u8),
	EQC(u8),
	GEC(u8),
	GTC(u8),
	NEC(u8),
	LTR(u8),
	LER(u8),
	EQR(u8),
	GER(u8),
	GTR(u8),
	NER(u8),
}

impl Not for Cop {
	type Output = Cop;
	fn not(self) -> Self {
		match self {
			Cop::LTC(x) => Cop::GEC(x),
			Cop::LEC(x) => Cop::GTC(x),
			Cop::EQC(x) => Cop::NEC(x),
			Cop::GEC(x) => Cop::LTC(x),
			Cop::GTC(x) => Cop::LEC(x),
			Cop::NEC(x) => Cop::EQC(x),
			Cop::LTR(x) => Cop::GER(x),
			Cop::LER(x) => Cop::GTR(x),
			Cop::EQR(x) => Cop::NER(x),
			Cop::GER(x) => Cop::LTR(x),
			Cop::GTR(x) => Cop::LER(x),
			Cop::NER(x) => Cop::EQR(x),
		}
	}
}

fn parse_a(s: &[u8]) -> u8 {
	let c = s[0];
	if c == b'\\' {
		match s[1] {
			c2 @ b'0'...b'9' => c2 - b'0',
			b't' => b'\t',
			b'r' => b'\r',
			b'n' => b'\n',
			c2 => c2,
		}
	} else {
		c
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
			b'0'...b'9' => (c - b'0') as usize,
			b'a'...b'f' => (10 + (c - b'a')) as usize,
			b'A'...b'F' => (10 + (c - b'A')) as usize,
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
			b'0'...b'9' => ret += m * (c - b'0') as usize,
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
				let ret = match chs[idx + 1] {
					b'a' => parse_a(&indice),
					b'b' => parse_b(&indice),
					b'd' => parse_d(&indice),
					b'h' => parse_h(&indice),
					_ => panic!("Unknown numerical mode in deref"),
				};
				return (&chs[idx + 2..], ret);
			}
			x => indice.push(x),
		}
		idx += 1;
	}
}

fn parse_imm(chs: &[u8]) -> (&[u8], u8) {
	let mut idx = 0;
	while chs[idx] != b'(' {
		idx += 1
	}
	let (chs, val) = parse_reg(&chs[idx + 1..]);
	if chs[0] != b')' {
		panic!("Expected )");
	}
	(&chs[1..], val)
}

fn parse_str(chs: &[u8]) -> (&[u8], Vec<u8>) {
	let mut idx = 0;
	let mut indice = Vec::new();
	while chs[idx] != b'(' {
		idx += 1
	}
	idx += 1;
	while chs[idx] != b':' && chs[idx + 1] != b'a' && chs[idx + 2] != b')' {
		indice.push(chs[idx]);
		idx += 1;
	}
	(&chs[idx + 3..], indice)
}

fn parse_mem<'a, 'b>(chs: &'a [u8], parser: &'b mut Parser) -> &'a [u8] {
	let mut idx = 0;
	let mut indices = Vec::new();
	let mut indice = Vec::new();
	while chs[idx] != b'[' {
		idx += 1
	}
	idx += 1;
	loop {
		match chs[idx] {
			b'-' => {
				let ind = mem::replace(&mut indice, Vec::new());
				indices.push(ind);
			}
			b':' => {
				let ind = mem::replace(&mut indice, Vec::new());
				indices.push(ind);
				idx += 1;
				let mut indvals = match chs[idx] {
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
							}
							_ => panic!("Unexpected deref mode"),
						}
					}
					b']' => (),
					_ => panic!("Expected : or ]"),
				}
				if !regref {
					while !indvals.is_empty() && indvals[0] == 0 {
						indvals.remove(0);
					}
				}
				for ind in indvals {
					if regref {
						parser.prog.push(Instr::Reg2MR(ind));
					} else {
						parser.prog.push(Instr::Imm2MR(ind));
					}
				}
				return &chs[idx + 1..];
			}
			x => indice.push(x),
		}
		idx += 1;
	}
}

fn parse_lab(chs: &[u8]) -> (&[u8], Vec<u8>) {
	let mut idx = 0;
	while chs[idx] != b'{' {
		idx += 1
	}
	idx += 1;
	let mut label = Vec::new();
	while chs[idx] != b'}' {
		label.push(chs[idx]);
		idx += 1;
	}
	(&chs[idx + 1..], label)
}

fn parse_cmp(mut chs: &[u8]) -> (&[u8], Cmp) {
	chs = skipws(chs);
	match (chs[0], chs[1]) {
		(b'<', b'=') => (&chs[2..], Cmp::LE),
		(b'<', _) => (&chs[1..], Cmp::LT),
		(b'=', _) => (&chs[1..], Cmp::EQ),
		(b'>', b'=') => (&chs[2..], Cmp::GE),
		(b'>', _) => (&chs[1..], Cmp::GT),
		(b'!', _) => (&chs[1..], Cmp::NE),
		_ => panic!("Expected > | >= | == | <= | < | !"),
	}
}

fn parse_cond(chs: &[u8]) -> (&[u8], Cop) {
	let mut idx = 0;
	while chs[idx] != b'(' {
		idx += 1
	}
	idx += 1;
	while chs[idx] == b' ' || chs[idx] == b'\t' {
		idx += 1
	}
	let (newchs, cop) = parse_cmp(&chs[idx..]);
	let chs = skipws(newchs);
	let (newchs, v) = parse_reg(chs);
	if newchs[0] == b')' {
		(
			&newchs[1..],
			match cop {
				Cmp::LT => Cop::LTC(v),
				Cmp::LE => Cop::LEC(v),
				Cmp::EQ => Cop::EQC(v),
				Cmp::GT => Cop::GTC(v),
				Cmp::GE => Cop::GEC(v),
				Cmp::NE => Cop::NEC(v),
			},
		)
	} else if newchs[0] == b':' && newchs[1] == b'r' && newchs[2] == b')' {
		(
			&newchs[3..],
			match cop {
				Cmp::LT => Cop::LTR(v),
				Cmp::LE => Cop::LER(v),
				Cmp::EQ => Cop::EQR(v),
				Cmp::GT => Cop::GTR(v),
				Cmp::GE => Cop::GER(v),
				Cmp::NE => Cop::NER(v),
			},
		)
	} else {
		panic!("Expected ) or :r)")
	}
}

macro_rules! rereg {
	() => {
		r"(.:a|\\.:a|[01]{8}:b|\d{1,3}:d|[0-9a-fA-F]{1,2}:h)"
	};
}
macro_rules! reconst {
	() => {
		concat!(r"\(", rereg!(), r"\)")
	};
}
macro_rules! remem {
	() => { r"(\[((.|\\.)(-(.|\\.))*:a|[01]{1,8}(-[01]{1,8})*:b|\d{1,3}(-\d{1,3})*:d|[0-9a-fA-F]{1,2}(-[0-9a-fA-F]{1,2})*:h)(:r)?\])" }
}
macro_rules! relab {
	() => {
		r"\{[a-zA-Z0-9_]+\}"
	};
}
macro_rules! recond {
	() => {
		concat!(r"\((>|<|>=|<=|!|=)\s*", rereg!(), r"(:r)?\)")
	};
}

const RULES: &'static [&'static str] = &[
	concat!(r"^!s?\s*", remem!()),
	concat!(r"^\*s\s*", remem!()),
	concat!(r"^\*\s*", remem!()),
	concat!(r"^\*\s*", rereg!()),
	concat!(r#"^"\s*"#, remem!()),
	concat!(r#"^"\s*"#, rereg!()),
	concat!("^", rereg!(), r"\s*>\s*", remem!()),
	concat!("^", rereg!(), r"\s*<\s*", remem!()),
	concat!("^", rereg!(), r"\s*<>\s*", remem!()),
	concat!(r"^@\s*", relab!()),
	concat!(r"^#\s*", relab!()),
	concat!(
		r"^\?\s*",
		rereg!(),
		r"\s*",
		recond!(),
		r"\s*_\s*",
		relab!(),
		r"\s*_\s*\(\s*",
		relab!(),
		r"\)"
	),
	concat!(r"^\?\s*", rereg!(), r"\s*", recond!(), r"\s*_\s*", relab!()),
	concat!(r"^%\s*", relab!(), r"\s*", recond!(), r"\s*", reconst!()),
	concat!(r"^\\s*", rereg!(), r"\s*", reconst!()),
	concat!(r"^\$\s*", remem!(), reconst!()),
	concat!(r"^\$s\s*", remem!(), r"\s*\(.*:a\)"),
	concat!(r"^\.\s*", rereg!(), r"\s*", rereg!()),
	concat!(r"^,\s*", rereg!(), r"\s*", rereg!()),
	concat!(r"^\+\s*", rereg!()),
	concat!(r"^-\s*", rereg!()),
	concat!(r"^\+\+\s*", rereg!(), r"(\s*/\s*", rereg!(), r")+"),
	concat!(r"^--\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r"^\*\*\s*", rereg!(), r"(\s*/\s*", rereg!(), r")+"),
	concat!(r"^\\\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r"^\^\^\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r"^<<\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r"^<<<\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r"^>>\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r"^>>>\s*", rereg!(), r"\s*/\s*", rereg!()),
	concat!(r"^&\s*", rereg!(), r"(\s*/\s*", rereg!(), r")+"),
	concat!(r"^\|\s*", rereg!(), r"(\s*/\s*", rereg!(), r")+"),
	concat!(r"^\^\s*", rereg!(), r"(\s*/\s*", rereg!(), r")+"),
	concat!(r"^~\s*", relab!()),
	concat!(r"^//"),
	concat!(r"^/"),
	concat!(r"^;[^']*"),
	concat!(r"^'.*"),
	concat!(r"^\?\s*", rereg!(), r"\s*", recond!(), r"\s*_\s*"),
];

#[derive(Debug)]
pub struct Parser {
	pub prog: Vec<Instr>,
	pub labels: FnvHashMap<Vec<u8>, usize>,
	pub labelstr: FnvHashMap<Vec<u8>, Vec<u8>>,
	regset: RegexSet,
	regtest: Regex,
}

impl Default for Parser {
	fn default() -> Parser {
		Parser {
			prog: Vec::new(),
			labels: FnvHashMap::default(),
			labelstr: FnvHashMap::default(),
			regset: RegexSet::new(RULES).unwrap(),
			regtest: Regex::new(concat!("^", rereg!())).unwrap(),
		}
	}
}

fn skipws(s: &[u8]) -> &[u8] {
	let mut idx = 0;
	while {
		if idx == s.len() {
			return &[];
		}
		s[idx] == b' ' || s[idx] == b'\t'
	} {
		idx += 1;
	}
	&s[idx..]
}

impl Parser {
	pub fn parse_line(&mut self, mut line: &[u8]) {
		if line.is_empty() {
			return;
		}
		let cur_label = if line[0] == b'{' {
			let mut idx = 0;
			let mut lab = Vec::new();
			loop {
				idx += 1;
				let c = line[idx];
				match c {
					b'a'...b'z' | b'A'...b'Z' | b'0'...b'9' | b'_' => lab.push(c),
					b'}' => break,
					_ => panic!("Unterminated label"),
				}
			}
			let pglen = self.prog.len();
			if self.labels.insert(lab.clone(), pglen).is_some() {
				println!("Duplicate label");
			}
			line = &line[idx + 1..];
			lab
		} else {
			Vec::new()
		};
		while let Some(newline) = self.parse_instr(line, &cur_label) {
			line = newline;
		}
	}

	fn parse_instr<'a, 'b>(
		&'b mut self,
		mut line: &'a [u8],
		cur_label: &'b [u8],
	) -> Option<&'a [u8]> {
		if let Some(mid) = {
			line = skipws(line);
			self.regset.matches(line).into_iter().min()
		} {
			match mid {
				0 => {
					// !s mem, s mem
					let isstr = line[1] == b's';
					line = skipws(&line[if isstr { 2 } else { 1 }..]);
					line = parse_mem(line, self);
					self.prog
						.push(if isstr { Instr::GSMem } else { Instr::GCMem });
				}
				1 => {
					// *s mem
					line = skipws(&line[2..]);
					line = parse_mem(line, self);
					self.prog.push(Instr::PSMem);
				}
				2 => {
					// * mem
					line = skipws(&line[1..]);
					line = parse_mem(line, self);
					self.prog.push(Instr::PCMem);
				}
				3 => {
					// * reg
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::PCReg(r));
				}
				4 => {
					// " mem
					line = skipws(&line[1..]);
					line = parse_mem(line, self);
					self.prog.push(Instr::PNMem);
				}
				5 => {
					// " reg
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::PNReg(r));
				}
				6 => {
					// reg > mem
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					line = skipws(&line[1..]);
					line = parse_mem(line, self);
					self.prog.push(Instr::Reg2Mem(r));
				}
				7 => {
					// reg < mem
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					line = skipws(&line[1..]);
					line = parse_mem(line, self);
					self.prog.push(Instr::Mem2Reg(r));
				}
				8 => {
					// reg <> mem
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					line = skipws(&line[2..]);
					line = parse_mem(line, self);
					self.prog.push(Instr::SwapMem(r));
				}
				9 => {
					// @ lab
					line = skipws(&line[1..]);
					let (newline, lab) = parse_lab(line);
					line = newline;
					self.prog.push(Instr::Jmp(lab));
				}
				10 => {
					// # lab
					line = skipws(&line[1..]);
					let (newline, lab) = parse_lab(line);
					line = newline;
					self.prog.push(Instr::Call(lab));
				}
				11 => {
					// ? reg cond _ lab _ lab
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					let (newline, cond) = parse_cond(line);
					line = skipws(newline);
					line = skipws(&line[1..]);
					let (newline, lab1) = parse_lab(line);
					line = skipws(newline);
					line = skipws(&line[1..]);
					let (newline, lab2) = parse_lab(line);
					line = newline;
					self.prog.push(Instr::Cmp(cond, r, lab1));
					self.prog.push(Instr::Jmp(lab2));
				}
				12 => {
					// ? reg cond _ lab
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					let (newline, cond) = parse_cond(line);
					line = skipws(newline);
					line = skipws(&line[1..]);
					let (newline, lab1) = parse_lab(line);
					line = skipws(newline);
					if !line.is_empty() && line[0] == b'_' {
						line = &line[1..];
					}
					self.prog.push(Instr::Cmp(cond, r, lab1));
				}
				13 => {
					// % lab reg cond const
					line = skipws(&line[1..]);
					let (newline, lab) = parse_lab(line);
					line = skipws(newline);
				}
				14 => {
					// \ reg const
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					let (newline, c) = parse_imm(line);
					line = newline;
					self.prog.push(Instr::Imm2Reg(c, r));
				}
				15 => {
					// $ mem const
					line = skipws(&line[1..]);
					line = parse_mem(line, self);
					line = skipws(line);
					let (newline, c) = parse_imm(line);
					line = newline;
					self.prog.push(Instr::Imm2Mem(c));
				}
				16 => {
					// $s mem string
					line = skipws(&line[1..]);
					line = parse_mem(line, self);
					line = skipws(line);
					let (newline, cs) = parse_str(line);
					line = newline;
					self.prog.push(Instr::Str2Mem(cs));
				}
				17 => {
					// . reg reg
					line = skipws(&line[1..]);
					let (newline, r1) = parse_reg(line);
					line = skipws(newline);
					let (newline, r2) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::Reg2Reg(r2, r1));
				}
				18 => {
					// , reg reg
					line = skipws(&line[1..]);
					let (newline, r1) = parse_reg(line);
					line = skipws(newline);
					let (newline, r2) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::SwapReg(r1, r2));
				}
				19 => {
					// + reg
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::Incr(r));
				}
				20 => {
					// - reg
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::Decr(r));
				}
				21 => {
					// ++ reg reg..
					line = skipws(&line[2..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					let mut regs = vec![r];
					while !line.is_empty() && line[0] == b'/' {
						let templine = skipws(&line[1..]);
						if self.regtest.is_match(templine) {
							let (newline, r) = parse_reg(templine);
							line = skipws(newline);
							regs.push(r);
						} else {
							break;
						}
					}
					self.prog.push(Instr::Add(regs));
				}
				22 => {
					// -- reg reg
					line = skipws(&line[2..]);
					let (newline, r1) = parse_reg(line);
					line = skipws(newline);
					line = skipws(&newline[1..]);
					let (newline, r2) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::Sub(r2, r1));
				}
				23 => {
					// ** reg reg..
					line = skipws(&line[2..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					let mut regs = vec![r];
					while !line.is_empty() && line[0] == b'/' {
						let templine = skipws(&line[1..]);
						if self.regtest.is_match(templine) {
							let (newline, r) = parse_reg(templine);
							line = skipws(newline);
							regs.push(r);
						} else {
							break;
						}
					}
					self.prog.push(Instr::Mul(regs));
				}
				24 => {
					// \ reg reg
					line = skipws(&line[1..]);
					let (newline, r1) = parse_reg(line);
					line = skipws(newline);
					line = skipws(&newline[1..]);
					let (newline, r2) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::DivMod(r2, r1));
				}
				25 => {
					// ^^ reg reg
					line = skipws(&line[2..]);
					let (newline, r1) = parse_reg(line);
					line = skipws(newline);
					line = skipws(&newline[1..]);
					let (newline, r2) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::Exp(r2, r1));
				}
				26 => {
					// << reg reg
					line = skipws(&line[2..]);
					let (newline, r1) = parse_reg(line);
					line = skipws(newline);
					line = skipws(&newline[1..]);
					let (newline, r2) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::Shl(r2, r1));
				}
				27 => {
					// <<< reg reg
					line = skipws(&line[3..]);
					let (newline, r1) = parse_reg(line);
					line = skipws(newline);
					line = skipws(&newline[1..]);
					let (newline, r2) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::Rol(r2, r1));
				}
				28 => {
					// >> reg reg
					line = skipws(&line[2..]);
					let (newline, r1) = parse_reg(line);
					line = skipws(newline);
					line = skipws(&newline[1..]);
					let (newline, r2) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::Shr(r2, r1));
				}
				29 => {
					// >>> reg reg
					line = skipws(&line[3..]);
					let (newline, r1) = parse_reg(line);
					line = skipws(newline);
					line = skipws(&newline[1..]);
					let (newline, r2) = parse_reg(line);
					line = newline;
					self.prog.push(Instr::Ror(r2, r1));
				}
				30 => {
					// & reg reg..
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					let mut regs = vec![r];
					while !line.is_empty() && line[0] == b'/' {
						let templine = skipws(&line[1..]);
						if self.regtest.is_match(templine) {
							let (newline, r) = parse_reg(templine);
							line = skipws(newline);
							regs.push(r);
						} else {
							break;
						}
					}
					self.prog.push(Instr::And(regs));
				}
				31 => {
					// | reg reg..
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					let mut regs = vec![r];
					while !line.is_empty() && line[0] == b'/' {
						let templine = skipws(&line[1..]);
						if self.regtest.is_match(templine) {
							let (newline, r) = parse_reg(templine);
							line = skipws(newline);
							regs.push(r);
						} else {
							break;
						}
					}
					self.prog.push(Instr::Or(regs));
				}
				32 => {
					// ^ reg reg..
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					let mut regs = vec![r];
					while !line.is_empty() && line[0] == b'/' {
						let templine = skipws(&line[1..]);
						if self.regtest.is_match(templine) {
							let (newline, r) = parse_reg(templine);
							line = skipws(newline);
							regs.push(r);
						} else {
							break;
						}
					}
					self.prog.push(Instr::Xor(regs));
				}
				33 => {
					// ~ lab
					line = skipws(&line[1..]);
					let (newline, lab) = parse_lab(line);
					line = newline;
					self.prog.push(Instr::Print(lab));
				}
				34 => {
					// //
					line = &line[2..];
					self.prog.push(Instr::Halt);
				}
				35 => {
					// /
					line = &line[1..];
					self.prog.push(Instr::Return);
				}
				36 => {
					// ;
					line = &line[1..];
					let mut ls = Vec::new();
					while !line.is_empty() && line[0] != b'\'' {
						ls.push(line[0]);
						line = &line[1..];
					}
					self.labelstr.insert(Vec::from(cur_label), ls);
					return None;
				}
				37 => {
					// '
					return None;
				}
				38 => {
					// ? reg cond _ instr
					line = skipws(&line[1..]);
					let (newline, r) = parse_reg(line);
					line = skipws(newline);
					let (newline, cond) = parse_cond(line);
					line = skipws(newline);
					line = skipws(&line[1..]);
					let lab = self.mklab();
					self.prog.push(Instr::Cmp(!cond, r, lab.clone()));
					if let Some(newline) = self.parse_instr(line, cur_label) {
						line = skipws(newline);
						let len = self.prog.len();
						self.labels.insert(lab, len);
						if !line.is_empty() && line[0] == b'_' {
							let lab2 = self.mklab();
							self.prog.push(Instr::Jmp(lab2.clone()));
							line = skipws(&line[1..]);
							if line[0] == b'{' {
								line = &line[1..];
								let mut jlab = Vec::new();
								while line[0] != b'}' {
									jlab.push(line[0]);
									line = &line[1..];
								}
								self.prog.push(Instr::Jmp(jlab));
								line = &line[1..];
							} else if let Some(newline) = self.parse_instr(line, cur_label) {
								line = newline;
							} else {
								panic!("Expected instruction in else branch")
							}
							let len = self.prog.len();
							self.labels.insert(lab2, len);
						}
					} else {
						panic!("Expected instruction")
					}
				}
				_ => unreachable!(),
			}
			Some(line)
		} else {
			None
		}
	}

	fn mklab(&self) -> Vec<u8> {
		let mut lab = self.prog.len().to_string().into_bytes();
		lab.push(b'$');
		lab
	}

	pub fn run(&self, mem: &mut [u8]) {
		let mut regs = [0u8; 256];
		let mut mptr = 0;
		let mut callstack = Vec::new();
		self.runcore(0, &mut regs, mem, &mut mptr, &mut callstack)
	}

	fn runcore(
		&self,
		mut pc: usize,
		regs: &mut [u8; 256],
		mem: &mut [u8],
		mptr: &mut usize,
		callstack: &mut Vec<usize>,
	) {
		while pc < self.prog.len() {
			match self.prog[pc] {
				Instr::Reg2Reg(r2, r1) => regs[r1 as usize] = regs[r2 as usize],
				Instr::Reg2Mem(r) => {
					mem[*mptr] = regs[r as usize];
					*mptr = 0;
				}
				Instr::Mem2Reg(r) => {
					regs[r as usize] = mem[*mptr];
					*mptr = 0;
				}
				Instr::Reg2MR(r) => {
					*mptr <<= 8;
					*mptr |= regs[r as usize] as usize;
				}
				Instr::Imm2MR(v) => {
					*mptr <<= 8;
					*mptr |= v as usize;
				}
				Instr::SwapMem(r) => {
					let t = regs[r as usize];
					regs[r as usize] = mem[*mptr];
					mem[*mptr] = t;
				}
				Instr::SwapReg(r1, r2) => {
					regs.swap(r1 as usize, r2 as usize);
				}
				Instr::Imm2Reg(v, r) => regs[r as usize] = v,
				Instr::Imm2Mem(v) => {
					mem[*mptr] = v;
					*mptr = 0;
				}
				Instr::Str2Mem(ref vs) => {
					for &v in vs.iter() {
						mem[*mptr] = v;
						*mptr += 1;
					}
					*mptr = 0;
				}
				Instr::PCReg(r) => {
					print!("{}", regs[r as usize] as char);
				}
				Instr::PNReg(r) => {
					print!("{}", regs[r as usize]);
				}
				Instr::Incr(r) => regs[r as usize] = regs[r as usize].wrapping_add(1),
				Instr::Decr(r) => regs[r as usize] = regs[r as usize].wrapping_sub(1),
				Instr::Add(ref rs) => {
					let mut v = 0u8;
					for r in rs.iter().cloned() {
						v = v.wrapping_add(regs[r as usize]);
					}
					regs[*rs.last().unwrap() as usize] = v;
				}
				Instr::Sub(r2, r1) => {
					regs[r1 as usize] = regs[r1 as usize].wrapping_sub(regs[r2 as usize])
				}
				Instr::Mul(ref rs) => {
					let mut v = 1u8;
					for r in rs.iter().cloned() {
						v = v.wrapping_mul(regs[r as usize]);
					}
					regs[*rs.last().unwrap() as usize] = v;
				}
				Instr::DivMod(r2, r1) => {
					let d = regs[r1 as usize] / regs[r2 as usize];
					let m = regs[r1 as usize] % regs[r2 as usize];
					regs[r1 as usize] = d;
					regs[r2 as usize] = m;
				}
				Instr::Exp(r2, r1) => {
					let mut v = 1u8;
					for _ in 0..regs[r2 as usize] {
						v = v.wrapping_mul(regs[r1 as usize]);
					}
					regs[r1 as usize] = v;
				}
				Instr::And(ref rs) => {
					let mut v = 255u8;
					for r in rs.iter().cloned() {
						v &= regs[r as usize];
					}
					regs[*rs.last().unwrap() as usize] = v;
				}
				Instr::Or(ref rs) => {
					let mut v = 0u8;
					for r in rs.iter().cloned() {
						v |= regs[r as usize];
					}
					regs[*rs.last().unwrap() as usize] = v;
				}
				Instr::Xor(ref rs) => {
					let mut v = 0u8;
					for r in rs.iter().cloned() {
						v ^= regs[r as usize];
					}
					regs[*rs.last().unwrap() as usize] = v;
				}
				Instr::Shl(r2, r1) => {
					regs[r1 as usize] = regs[r1 as usize].wrapping_shl(regs[r2 as usize] as u32)
				}
				Instr::Shr(r2, r1) => {
					regs[r1 as usize] = regs[r1 as usize].wrapping_shr(regs[r2 as usize] as u32)
				}
				Instr::Rol(r2, r1) => {
					regs[r1 as usize] = regs[r1 as usize].rotate_left(regs[r2 as usize] as u32)
				}
				Instr::Ror(r2, r1) => {
					regs[r1 as usize] = regs[r1 as usize].rotate_right(regs[r2 as usize] as u32)
				}
				Instr::PCMem => {
					print!("{}", mem[*mptr] as char);
					*mptr = 0;
				}
				Instr::PNMem => {
					print!("{}", mem[*mptr]);
					*mptr = 0;
				}
				Instr::PSMem => {
					let mut out = String::new();
					while mem[*mptr] != 0 {
						out.push(mem[*mptr] as char);
						*mptr += 1;
					}
					print!("{}", out);
					*mptr = 0;
				}
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
				}
				Instr::Call(ref lab) => {
					if let Some(&loc) = self.labels.get(lab) {
						callstack.push(pc);
						pc = loc;
						continue;
					} else {
						println!("Undefined label: {:?}", lab);
					}
				}
				Instr::Jmp(ref lab) => {
					if let Some(&loc) = self.labels.get(lab) {
						pc = loc;
						continue;
					} else {
						println!("Undefined label: {:?}", lab);
					}
				}
				Instr::GCMem => {
					mem[*mptr] = stdin().bytes().next().unwrap_or(Ok(0)).unwrap();
					*mptr = 0;
				}
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
				}
				Instr::Cmp(op, r1, ref lab) => {
					let r1 = regs[r1 as usize];
					if match op {
						Cop::LTC(v) => r1 < v,
						Cop::LEC(v) => r1 <= v,
						Cop::EQC(v) => r1 == v,
						Cop::GEC(v) => r1 >= v,
						Cop::GTC(v) => r1 > v,
						Cop::NEC(v) => r1 != v,
						Cop::LTR(v) => r1 < regs[v as usize],
						Cop::LER(v) => r1 <= regs[v as usize],
						Cop::EQR(v) => r1 == regs[v as usize],
						Cop::GER(v) => r1 >= regs[v as usize],
						Cop::GTR(v) => r1 > regs[v as usize],
						Cop::NER(v) => r1 != regs[v as usize],
					} {
						if let Some(&loc) = self.labels.get(lab) {
							pc = loc;
							continue;
						} else {
							println!("Undefined label: {:?}", lab);
						}
					}
				}
				Instr::Loop(_, _, _, _) => {}
				Instr::Return => {
					if let Some(loc) = callstack.pop() {
						pc = loc;
					} else {
						println!("Callstack underflow");
					}
				}
				Instr::Halt => return,
			}
			pc += 1;
		}
	}
}
