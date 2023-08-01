pub mod repl {
    use crate::lexer::lexer::Lexer;
    use crate::token::token::TokenType;
    use std::io::{self, Write};

    const PROMPT: &str = ">>> ";

    pub fn start() {
        let mut quit_loop = false;
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
            loop {
                let token = lexer.next_token();
                match token.token_type {
                    TokenType::EOF => break,
                    _ => {
                        if token.token_type == TokenType::IDENT && token.literal == "q".to_string()
                        {
                            quit_loop = true;
                        }
                        println!("{:?}", token);
                    }
                }
            }
            if quit_loop {
                break;
            }
        }
    }
}
