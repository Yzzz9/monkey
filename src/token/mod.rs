#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub enum TokenType {
    UNKNOWN,
    ILLEGAL,
    EOF,

    // identifiers + literals
    IDENT(String), // add, foobar, x, y, ...
    INT(i32),   // 12345
    BOOL(bool),
    STRING(String),

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
    COLON,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPARAN, // (
    RPARAN,
    LBRACE, // {
    RBRACE,
    LBRACKET,
    RBRACKET,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}
