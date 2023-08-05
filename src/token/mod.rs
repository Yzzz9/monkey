#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}
impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type,
            literal,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // identifiers + literals
    IDENT, // add, foobar, x, y, ...
    INT,   // 12345

    // Operators
    BANG,
    ASSIGN,
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    EQ,
    NOTEQ,
    LT,
    GT,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPARAN,
    RPARAN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}
