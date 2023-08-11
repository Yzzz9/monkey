use std::collections::HashMap;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

type PrefixParseFn = fn(&mut Parser) -> Box<dyn Expression>;
type InfixParseFn = fn(&mut Parser, Option<Box<dyn Expression>>) -> Box<dyn Expression>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Consts {
    LOWEST = 1,
    EQUALS = 2,
    LESSGREATER = 3,
    SUM = 4,
    PRODUCT = 5,
    PREFIX = 6,
    CALL = 7,
}
pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
    pub precedences: HashMap<TokenType, Consts>,
}
impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut returning_parser = Parser {
            lexer,
            curr_token: Token::new(TokenType::EOF, "\0".to_string()),
            peek_token: Token::new(TokenType::EOF, "\0".to_string()),
            errors: vec![],
            infix_parse_fns: HashMap::new(),
            prefix_parse_fns: HashMap::new(),
            precedences: HashMap::from([
                (TokenType::EQ, Consts::EQUALS),
                (TokenType::NOTEQ, Consts::EQUALS),
                (TokenType::LT, Consts::LESSGREATER),
                (TokenType::GT, Consts::LESSGREATER),
                (TokenType::PLUS, Consts::SUM),
                (TokenType::MINUS, Consts::SUM),
                (TokenType::SLASH, Consts::PRODUCT),
                (TokenType::ASTERISK, Consts::PRODUCT),
            ]),
        };
        returning_parser.register_prefix(TokenType::IDENT, Parser::parse_identifier);
        returning_parser.register_prefix(TokenType::INT, Parser::parse_integer_literal);
        returning_parser.register_prefix(TokenType::BANG, Parser::parse_prefix_expression);
        returning_parser.register_prefix(TokenType::MINUS, Parser::parse_prefix_expression);

        returning_parser.register_infix(TokenType::PLUS, Parser::parse_infix_expression);
        returning_parser.register_infix(TokenType::MINUS, Parser::parse_infix_expression);
        returning_parser.register_infix(TokenType::ASTERISK, Parser::parse_infix_expression);
        returning_parser.register_infix(TokenType::SLASH, Parser::parse_infix_expression);
        returning_parser.register_infix(TokenType::EQ, Parser::parse_infix_expression);
        returning_parser.register_infix(TokenType::NOTEQ, Parser::parse_infix_expression);
        returning_parser.register_infix(TokenType::LT, Parser::parse_infix_expression);
        returning_parser.register_infix(TokenType::GT, Parser::parse_infix_expression);

        returning_parser.next_token();
        returning_parser.next_token();
        returning_parser
    }
    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }
    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }
    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }
    pub fn parse_program(&mut self) -> Option<Program> {
        println!("inside parse_program................");
        let mut program = Program::new();

        while self.curr_token.token_type != TokenType::EOF {
            println!("curr_token = {:?}", self.curr_token.token_type);
            let stmt = self.parse_statement();

            if stmt.is_some() {
                program.statements.push(stmt.unwrap());
            }
            self.next_token();
        }
        Some(program)
    }
    fn parse_statement(&mut self) -> Option<Box<dyn Node>> {
        println!("inside parse_statement................");
        match self.curr_token.token_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }
    fn parse_let_statement(&mut self) -> Option<Box<dyn Node>> {
        println!("inside parse_let_statement................");
        let mut stmt = LetStatement {
            token: self.curr_token.clone(),
            name: Identifier {
                token: self.curr_token.clone(),
                value: "".to_string(),
            },
            value: Some(Box::new(Identifier {
                token: self.curr_token.clone(),
                value: "".to_string(),
            })), // could put None here
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

        // TODO - We're skipping expression until we encounter a semicolon
        while !self.curr_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Box::new(stmt))
    }
    fn parse_return_statement(&mut self) -> Option<Box<dyn Node>> {
        println!("inside parse_return_statement................");
        let stmt = ReturnStatement {
            token: self.curr_token.clone(),
            return_value: Some(Box::new(Identifier {
                token: self.curr_token.clone(),
                value: "".to_string(),
            })), // could use None here
        };

        self.next_token();

        // TODO - We're skipping expression until we encounter a semicolon
        while !self.curr_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Box::new(stmt))
    }
    fn parse_expression_statement(&mut self) -> Option<Box<dyn Node>> {
        println!("inside parse_expression_statement................");
        let stmt = ExpressionStatement {
            token: self.curr_token.clone(),
            expression: self.parse_expression(Consts::LOWEST),
        };
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Box::new(stmt))
    }
    fn parse_expression(&mut self, precedence: Consts) -> Option<Box<dyn Expression>> {
        println!("inside parse_expression................");
        println!("prefix_parse_fns = {:?}", self.prefix_parse_fns);
        let prefix = self.prefix_parse_fns.get(&self.curr_token.token_type);
        if prefix.is_none() {
            self.no_prefix_parse_fn_error(self.curr_token.token_type);
            return None;
        }
        let prefix = prefix.unwrap();
        let mut left_exp = prefix(self);

        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            // this is very dirty code writing
            // because immutable borrow -> mutable borrow -> immutable borrow occurs here
            // Try to fix it later -> TODO
            #[allow(unused_assignments)]
            let mut infix_fn: InfixParseFn = Parser::parse_infix_expression;
            {
                let infix = self.infix_parse_fns.get(&self.peek_token.token_type);
                if infix.is_none() {
                    return Some(left_exp);
                }

                infix_fn = *infix.unwrap();
            }
            self.next_token();
            left_exp = infix_fn(self, Some(left_exp));
        }

        Some(left_exp)
    }
    fn parse_prefix_expression(&mut self) -> Box<dyn Expression> {
        let token = self.curr_token.clone();
        let operator = self.curr_token.literal.clone();
        self.next_token();
        Box::new(PrefixExpression {
            token,
            operator,
            right: self.parse_expression(Consts::PREFIX),
        })
    }
    fn parse_infix_expression(&mut self, left: Option<Box<dyn Expression>>) -> Box<dyn Expression> {
        let token = self.curr_token.clone();
        let operator = self.curr_token.literal.clone();
        let precedence = self.curr_precedence();
        self.next_token();
        Box::new(InfixExpression {
            token,
            operator,
            left,
            right: self.parse_expression(precedence),
        })
    }
    fn parse_identifier(&mut self) -> Box<dyn Expression> {
        println!("inside parse_identifier................");
        Box::new(Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        })
    }
    fn parse_integer_literal(&mut self) -> Box<dyn Expression> {
        let value = self.curr_token.literal.clone().parse::<i64>();
        let mut final_value = 0 as i64;
        match value {
            Ok(x) => {
                final_value = x;
            }
            Err(_) => {
                let msg = format!("Could not parse {} as integer", self.curr_token.literal);
                self.errors.push(msg);
            }
        }
        Box::new(IntegerLiteral {
            token: self.curr_token.clone(),
            value: final_value,
        })
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
    fn no_prefix_parse_fn_error(&mut self, token_type: TokenType) {
        let msg = format!("no prefix parse function for {:?} found", token_type);
        self.errors.push(msg);
    }
    fn peek_error(&mut self, token: TokenType) {
        let msg = format!(
            "Expected next token to be {:?}, got {:?} instead",
            token, self.peek_token.token_type
        );
        self.errors.push(msg);
    }
    fn peek_precedence(&self) -> Consts {
        match self.precedences.get(&self.peek_token.token_type) {
            Some(x) => (*x).clone(),
            None => Consts::LOWEST,
        }
    }
    fn curr_precedence(&self) -> Consts {
        match self.precedences.get(&self.curr_token.token_type) {
            Some(x) => (*x).clone(),
            None => Consts::LOWEST,
        }
    }
    fn curr_token_is(&self, token: TokenType) -> bool {
        self.curr_token.token_type == token
    }
    fn peek_token_is(&self, token: TokenType) -> bool {
        self.peek_token.token_type == token
    }
}
