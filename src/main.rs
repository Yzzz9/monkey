mod ast;
mod lexer;
mod parser;
mod repl;
mod token;
fn main() {
    println!("This is interpreter....");
    repl::start();
}
