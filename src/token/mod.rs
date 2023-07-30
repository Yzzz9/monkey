pub mod token {
    #[derive(Debug, PartialEq)]
    pub struct TokenType(pub String);

    #[derive(Debug, PartialEq)]
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

    pub const ILLEGAL: &str = "ILLEGAL";
    pub const EOF: &str = "EOF";

    // identifiers + literals
    pub const IDENT: &str = "IDENT"; // add, foobar, x, y, ...
    pub const INT: &str = "INT"; // 12345

    // Operators
    pub const ASSIGN: &str = "=";
    pub const PLUS: &str = "+";

    // Delimiters
    pub const COMMA: &str = ",";
    pub const SEMICOLON: &str = ";";

    pub const LPARAN: &str = "(";
    pub const RPARAN: &str = ")";
    pub const LBRACE: &str = "{";
    pub const RBRACE: &str = "}";

    // Keywords
    pub const FUNCTION: &str = "FUNCTION";
    pub const LET: &str = "LET";
}
