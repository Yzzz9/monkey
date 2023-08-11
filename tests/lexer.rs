use monkey::lexer::Lexer;
use monkey::token::*;

#[test]
fn lexer_basic_test() {
    let input = "=+(){},;".to_string();
    let mut lexer = Lexer::new(input);
    let tests = vec![
        Token::new(TokenType::ASSIGN, "=".to_string()),
        Token::new(TokenType::PLUS, "+".to_string()),
        Token::new(TokenType::LPARAN, "(".to_string()),
        Token::new(TokenType::RPARAN, ")".to_string()),
        Token::new(TokenType::LBRACE, "{".to_string()),
        Token::new(TokenType::RBRACE, "}".to_string()),
        Token::new(TokenType::COMMA, ",".to_string()),
        Token::new(TokenType::SEMICOLON, ";".to_string()),
        Token::new(TokenType::EOF, "\0".to_string()),
    ];
    for test in tests {
        assert_eq!(test, lexer.next_token());
    }
}
