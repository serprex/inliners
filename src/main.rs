extern crate fnv;
extern crate regex;
mod parser;
use parser::*;
use std::env;
use std::fs;
use std::io::{BufRead, BufReader};

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
		parser.run(&mut [0u8; 65536]);
	} else {
		println!("inliners [filename]");
	}
}
