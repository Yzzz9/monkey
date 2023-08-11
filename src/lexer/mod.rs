use crate::token::{Token, TokenType};

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
    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            return b'\0';
        }
        self.input[self.read_position] as u8
    }
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let return_token = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    let next_ch = self.ch;
                    self.read_char();
                    Token::new(
                        TokenType::EQ,
                        format!("{}{}", next_ch as char, self.ch as char),
                    )
                } else {
                    Token::new(TokenType::ASSIGN, (self.ch as char).to_string())
                }
            }
            b'(' => Token::new(TokenType::LPARAN, (self.ch as char).to_string()),
            b')' => Token::new(TokenType::RPARAN, (self.ch as char).to_string()),
            b'{' => Token::new(TokenType::LBRACE, (self.ch as char).to_string()),
            b'}' => Token::new(TokenType::RBRACE, (self.ch as char).to_string()),
            b',' => Token::new(TokenType::COMMA, (self.ch as char).to_string()),
            b';' => Token::new(TokenType::SEMICOLON, (self.ch as char).to_string()),
            b'+' => Token::new(TokenType::PLUS, (self.ch as char).to_string()),
            b'-' => Token::new(TokenType::MINUS, (self.ch as char).to_string()),
            b'*' => Token::new(TokenType::ASTERISK, (self.ch as char).to_string()),
            b'/' => Token::new(TokenType::SLASH, (self.ch as char).to_string()),
            b'!' => {
                if self.peek_char() == b'=' {
                    let next_ch = self.ch;
                    self.read_char();
                    Token::new(
                        TokenType::NOTEQ,
                        format!("{}{}", next_ch as char, self.ch as char),
                    )
                } else {
                    Token::new(TokenType::BANG, (self.ch as char).to_string())
                }
            }
            b'<' => Token::new(TokenType::LT, (self.ch as char).to_string()),
            b'>' => Token::new(TokenType::GT, (self.ch as char).to_string()),
            b'\0' => Token::new(TokenType::EOF, (self.ch as char).to_string()),
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let token_type = lookup_ident(&literal);
                    return Token::new(token_type, literal);
                } else if is_digit(self.ch) {
                    let token_type = TokenType::INT;
                    let literal = self.read_number();
                    return Token::new(token_type, literal);
                } else {
                    Token::new(TokenType::ILLEGAL, (self.ch as char).to_string())
                }
            }
        };
        self.read_char();
        return_token
    }
}

fn is_letter(ch: u8) -> bool {
    (b'a' <= ch && ch <= b'z') || (b'A' <= ch && ch <= b'Z') || (ch == b'_')
}

fn is_digit(ch: u8) -> bool {
    b'0' <= ch && ch <= b'9'
}

fn lookup_ident(s: &str) -> TokenType {
    match s {
        "let" => TokenType::LET,
        "fn" => TokenType::FUNCTION,
        "if" => TokenType::IF,
        "else" => TokenType::ELSE,
        "return" => TokenType::RETURN,
        "true" => TokenType::TRUE,
        "false" => TokenType::FALSE,
        _ => TokenType::IDENT,
    }
}
