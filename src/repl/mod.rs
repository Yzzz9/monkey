use crate::lexer::Lexer;
use crate::parser::*;
use monkey::ast::{Program, Node};
use monkey::{evaluator::{eval, environment::Environment}};
use std::io::{self, Write};
use std::rc::*;
use core::cell::RefCell;

const PROMPT: &str = ">>> ";

pub fn start() {
    //let mut quit_loop = false;
    loop {
        print!("{}", PROMPT);
        io::stdout()
            .flush()
            .expect("Could not flush output in REPL");

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Could not read line in REPL");

        let mut lexer = Lexer::new(input.clone());
        /*
        loop {
            let token = lexer.next_token();
            match token.token_type {
                TokenType::EOF => break,
                _ => {
                    println!("{:?}", token);
                }
            }
        }
        */
        let mut parser = Parser::new(lexer);
        let program: Program = parser.parse_program().unwrap();
        if !parser.errors.is_empty() {
            println!("{:?}", parser.errors);
            continue;
        }
        let env = Rc::new(RefCell::new(Environment::new()));

        let evaluated = eval(Node::Program(program), env);
    }
}
