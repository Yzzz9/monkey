mod lexer;
mod repl;
mod token;
fn main() {
    println!("This is interpreter....");
    repl::repl::start();
}
