//use crate::token::{Token, TokenType};
mod lexer {
    use crate::token::token::{self, Token, TokenType};

    #[derive(Debug)]
    pub struct Lexer {
        input: Vec<u8>,
        position: usize,
        read_position: usize,
        ch: u8,
    }
    impl Lexer {
        pub fn new(input: String) -> Lexer {
            let mut lexer = Lexer {
                input: input.into_bytes(),
                position: 0,
                read_position: 0,
                ch: b'\0',
            };
            lexer.read_char();
            lexer
        }
        fn read_char(&mut self) {
            if self.read_position >= self.input.len() {
                self.ch = b'\0';
            } else {
                self.ch = self.input[self.read_position]
            }
            self.position = self.read_position;
            self.read_position += 1;
        }
        fn read_identifier(&mut self) -> String {
            let position = self.position;
            while is_letter(self.ch) {
                self.read_char();
            }
            String::from_utf8(self.input[position..self.position].to_vec()).unwrap()
        }
        fn read_number(&mut self) -> String {
            let position = self.position;
            while is_digit(self.ch) {
                self.read_char();
            }
            String::from_utf8(self.input[position..self.position].to_vec()).unwrap()
        }
        fn skip_whitespace(&mut self) {
            loop {
                match self.ch {
                    b' ' | b'\t' | b'\r' | b'\n' => self.read_char(),
                    _ => break,
                }
            }
        }
        pub fn next_token(&mut self) -> Token {
            self.skip_whitespace();
            let return_token = match self.ch {
                b'=' => new_token(
                    TokenType(token::ASSIGN.to_string()),
                    (self.ch as char).to_string(),
                ),
                b'(' => new_token(
                    TokenType(token::LPARAN.to_string()),
                    (self.ch as char).to_string(),
                ),
                b')' => new_token(
                    TokenType(token::RPARAN.to_string()),
                    (self.ch as char).to_string(),
                ),
                b'{' => new_token(
                    TokenType(token::LBRACE.to_string()),
                    (self.ch as char).to_string(),
                ),
                b'}' => new_token(
                    TokenType(token::RBRACE.to_string()),
                    (self.ch as char).to_string(),
                ),
                b',' => new_token(
                    TokenType(token::COMMA.to_string()),
                    (self.ch as char).to_string(),
                ),
                b';' => new_token(
                    TokenType(token::SEMICOLON.to_string()),
                    (self.ch as char).to_string(),
                ),
                b'+' => new_token(
                    TokenType(token::PLUS.to_string()),
                    (self.ch as char).to_string(),
                ),
                b'\0' => new_token(
                    TokenType(token::EOF.to_string()),
                    (self.ch as char).to_string(),
                ),
                _ => {
                    if is_letter(self.ch) {
                        let literal = self.read_identifier();
                        let token_type = TokenType(lookup_ident(&literal));
                        new_token(token_type, literal)
                    } else if is_digit(self.ch) {
                        let token_type = TokenType(token::INT.to_string());
                        let literal = self.read_number();
                        new_token(token_type, literal)
                    } else {
                        new_token(
                            TokenType(token::ILLEGAL.to_string()),
                            (self.ch as char).to_string(),
                        )
                    }
                }
            };
            self.read_char();
            return_token
        }
    }

    fn new_token(token: TokenType, literal: String) -> Token {
        Token {
            token_type: token,
            literal,
        }
    }

    fn is_letter(ch: u8) -> bool {
        (b'a' <= ch && ch <= b'z') || (b'A' <= ch && ch <= b'Z') || (ch == b'_')
    }

    fn is_digit(ch: u8) -> bool {
        b'0' <= ch && ch <= b'9'
    }

    fn lookup_ident(s: &str) -> String {
        match s {
            "let" => token::LET.to_string(),
            "fun" => token::FUNCTION.to_string(),
            _ => token::IDENT.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::Lexer;
    use crate::token::token::{Token, TokenType};
    use crate::token::*;

    #[test]
    fn add_test() {
        assert_eq!(2, 1 + 1);
    }

    #[test]
    fn basic_test() {
        let input = "=+(){},;".to_string();
        let mut lexer = Lexer::new(input);
        let tests = vec![
            Token::new(TokenType(token::ASSIGN.to_string()), "=".to_string()),
            Token::new(TokenType(token::PLUS.to_string()), "+".to_string()),
            Token::new(TokenType(token::LPARAN.to_string()), "(".to_string()),
            Token::new(TokenType(token::RPARAN.to_string()), ")".to_string()),
            Token::new(TokenType(token::LBRACE.to_string()), "{".to_string()),
            Token::new(TokenType(token::RBRACE.to_string()), "}".to_string()),
            Token::new(TokenType(token::COMMA.to_string()), ",".to_string()),
            Token::new(TokenType(token::SEMICOLON.to_string()), ";".to_string()),
            Token::new(TokenType(token::EOF.to_string()), "\0".to_string()),
        ];
        for test in tests {
            assert_eq!(test, lexer.next_token());
        }
    }
}
