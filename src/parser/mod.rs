use crate::ast::{Identifier, LetStatement, Node, Program};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
}
impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut returning_parser = Parser {
            lexer,
            curr_token: Token::new(TokenType::EOF, "\0".to_string()),
            peek_token: Token::new(TokenType::EOF, "\0".to_string()),
            errors: vec![],
        };
        returning_parser.next_token();
        returning_parser.next_token();
        returning_parser
    }
    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }
    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program::new();

        while self.curr_token.token_type != TokenType::EOF {
            let stmt = self.parse_statement();

            if stmt.is_some() {
                program.statements.push(stmt.unwrap());
            }
            self.next_token();
        }
        Some(program)
    }
    fn parse_statement(&mut self) -> Option<Box<dyn Node>> {
        match self.curr_token.token_type {
            TokenType::LET => self.parse_let_statement(),
            _ => None,
        }
    }
    fn parse_let_statement(&mut self) -> Option<Box<dyn Node>> {
        let mut stmt = LetStatement {
            token: self.curr_token.clone(),
            name: Identifier {
                token: self.curr_token.clone(),
                value: "".to_string(),
            },
            value: Box::new(Identifier {
                token: self.curr_token.clone(),
                value: "".to_string(),
            }),
        };

        if !self.peek_expect(TokenType::IDENT) {
            return None;
        }

        stmt.name = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        if !self.peek_expect(TokenType::ASSIGN) {
            return None;
        }

        while !self.curr_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Box::new(stmt))
    }
    fn peek_expect(&mut self, token: TokenType) -> bool {
        if self.peek_token_is(token) {
            self.next_token();
            return true;
        }
        self.peek_error(token);
        false
    }
    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }
    fn peek_error(&mut self, token: TokenType) {
        let msg = format!(
            "Expected next token to be {:?}, got {:?} instead",
            token, self.peek_token.token_type
        );
        self.errors.push(msg);
    }
    fn curr_token_is(&self, token: TokenType) -> bool {
        self.curr_token.token_type == token
    }
    fn peek_token_is(&self, token: TokenType) -> bool {
        self.peek_token.token_type == token
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{LetStatement, Node};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn parser_basic_test() {
        let input = "let x = 5;
        let y = 10;
        let foobar = 8383838;"
            .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(check_parse_errors(&parser), false);

        assert_eq!(program.is_some(), true);
        let program = program.unwrap();

        assert_eq!(program.statements.len(), 3);
        let tests = vec!["x".to_string(), "y".to_string(), "foobar".to_string()];

        for (i, test) in tests.into_iter().enumerate() {
            let stmt = &program.statements[i];
            assert_eq!(
                test_let_statement(
                    &(*stmt).as_any().downcast_ref::<LetStatement>().unwrap(),
                    test.clone()
                ),
                true
            );
        }
    }

    #[test]
    fn parser_negative_test() {
        let input = "let x 5;
        let = 10;
        let 8383838;"
            .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(check_parse_errors(&parser), false);

        assert_eq!(program.is_some(), true);
        let program = program.unwrap();

        assert_eq!(program.statements.len(), 3);
        let tests = vec!["x".to_string(), "y".to_string(), "foobar".to_string()];

        for (i, test) in tests.into_iter().enumerate() {
            let stmt = &program.statements[i];
            assert_eq!(
                test_let_statement(
                    &(*stmt).as_any().downcast_ref::<LetStatement>().unwrap(),
                    test.clone()
                ),
                true
            );
        }
    }

    fn test_let_statement(stmt: &LetStatement, name: String) -> bool {
        if stmt.token_literal() != "let".to_string() {
            return false;
        }
        if stmt.name.value != name {
            return false;
        }
        if stmt.name.token_literal() != name {
            return false;
        }
        true
    }

    fn check_parse_errors(parser: &Parser) -> bool {
        parser.errors.len() != 0
    }
}
