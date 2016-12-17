extern crate fnv;
extern crate regex;
//mod vm;
mod parser;
use std::env;
use std::fs;
use std::io::{BufReader, BufRead};
use parser::*;
//use vm::Vm;

fn main() {
	if let Some(a) = env::args().nth(1) {
		let mut parser = Parser::default();
		if let Ok(f) = fs::File::open(&a) {
			let f = BufReader::new(f);
			for line in f.lines() {
				if let Ok(line) = line {
					parser.parse_line(line.as_bytes());
				}
			}
		}
		println!("{:?}", parser);
		parser.run(&mut [0u8; 65536]);
		//let mut vm = Vm::default();
	} else {
		println!("inliners [filename]");
	}
}
