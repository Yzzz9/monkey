use std::collections::BTreeMap;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

type PrefixParseFn = for<'a> fn(&'a mut Parser) -> Result<Expression, ParserError>;
type InfixParseFn = for<'a> fn(&'a mut Parser, Box<Expression>) -> Result<Expression, ParserError>;
type ParserError = String;
type Errors = Vec<ParserError>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Consts {
    LOWEST = 1,
    EQUALS = 2,
    LESSGREATER = 3,
    SUM = 4,
    PRODUCT = 5,
    PREFIX = 6,
    CALL = 7,
    INDEX = 8,
}

fn get_precedence(token: &TokenType) -> Consts {
    match token {
        TokenType::EQ | TokenType::NOTEQ => Consts::EQUALS,
        TokenType::LT | TokenType::GT => Consts::LESSGREATER ,
        TokenType::PLUS | TokenType::MINUS => Consts::SUM ,
        TokenType::ASTERISK | TokenType::SLASH => Consts::PRODUCT ,
        TokenType::LPARAN => Consts::CALL ,
        TokenType::LBRACE => Consts::INDEX ,
        _ => Consts::LOWEST ,
    }
}

fn get_prefix_fn(token: &TokenType) -> Option<PrefixParseFn> {
    match token {
        TokenType::IDENT(_) => Some(Parser::parse_identifier_expression),
        TokenType::INT(_) => Some(Parser::parse_integer_expression),
        TokenType::BOOL(_) => Some(Parser::parse_boolean_expression),
        TokenType::STRING(_) => Some(Parser::parse_string_literal_expression),
        TokenType::BANG => Some(Parser::parse_prefix_expression),
        TokenType::MINUS => Some(Parser::parse_prefix_expression),
        TokenType::LPARAN => Some(Parser::parse_grouped_expression),
        TokenType::IF => Some(Parser::parse_if_expression),
        TokenType::FUNCTION => Some(Parser::parse_fn_literal_expression),
        TokenType::LBRACKET => Some(Parser::parse_array_literal_expression),
        TokenType::LBRACE => Some(Parser::parse_hash_literal_expression),
        _ => None,
    }
}

fn get_infix_fn(token: &TokenType) -> Option<InfixParseFn> {
    match token {
        TokenType::PLUS => Some(Parser::parse_infix_expression),
        TokenType::MINUS => Some(Parser::parse_infix_expression),
        TokenType::ASTERISK => Some(Parser::parse_infix_expression),
        TokenType::SLASH => Some(Parser::parse_infix_expression),
        TokenType::EQ => Some(Parser::parse_infix_expression),
        TokenType::NOTEQ => Some(Parser::parse_infix_expression),
        TokenType::LT => Some(Parser::parse_infix_expression),
        TokenType::GT => Some(Parser::parse_infix_expression),
        TokenType::LPARAN => Some(Parser::parse_call_expression),
        TokenType::LBRACKET => Some(Parser::parse_index_expression),
        _ => None,
    }
}

/*
pub fn eprint_parse_errors(errs: &ParseErrors) {
    for err in errs.iter() {
        eprintln!("parse error: {}", err);
    }
}
*/

fn fmt_token_error(expected: &TokenType, result: &TokenType) -> ParserError {
    format!(
        "expected {:?}, found {:?}",
        expected,
        result
    )
}

/*
pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
    pub precedences: HashMap<TokenType, Consts>,
}
*/

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    pub errors: Vec<ParserError>
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_token: Token::new(TokenType::UNKNOWN, String::new()),
            peek_token: Token::new(TokenType::UNKNOWN, String::new()),
            errors: Vec::new(),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Result<Program, Errors> {
        let mut program = Program{
            statements: Vec::new(),
        };

        while self.curr_token.token_type != TokenType::EOF {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(err) => self.errors.push(err),
            }
            self.next_token();
        }

        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.errors.clone())
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.curr_token.token_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let mut name = String::new();

        match &self.peek_token.token_type {
            TokenType::IDENT(identifier) => {
                name = identifier.to_owned();
                self.next_token();

                self.expect_peek(&TokenType::ASSIGN)?;

                self.next_token();

                let mut value = self.parse_expression(Consts::LOWEST)?;

                if let Expression::FnLiteral(ref mut expr) = value {
                    expr.name = name.clone();
                }

                if self.peek_token.token_type == TokenType::SEMICOLON {
                    self.next_token();
                }

                Ok(Statement::Let(LetStatement {
                    identifier: IdentifierExpression { name },
                    value,
                }))
            }
            token => Err(format!(
                "expected identifier, found {:?}",
                token
            )),
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token();

        let value = self.parse_expression(Consts::LOWEST)?;

        if self.peek_token.token_type == TokenType::SEMICOLON {
            self.next_token();
        }

        Ok(Statement::Return(ReturnStatement { value }))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.parse_expression(Consts::LOWEST);

        if self.peek_token.token_type == TokenType::SEMICOLON {
            self.next_token();
        }

        expression.map(|expr| Statement::Expression(ExpressionStatement { expr }))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut statements = Vec::new();

        self.next_token();

        while self.curr_token.token_type != TokenType::RBRACE 
            && self.curr_token.token_type != TokenType::EOF {
            if let Ok(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        BlockStatement { statements }
    }

    fn parse_expression(&mut self, precedence: Consts) -> Result<Expression, ParserError> {
        match get_prefix_fn(&self.curr_token.token_type) {
            Some(prefix_fn) => {
                let mut lhs = prefix_fn(self)?;

                while self.peek_token.token_type != TokenType::SEMICOLON
                    && (precedence.clone() as usize) < get_precedence(&self.peek_token.token_type) as usize
                {
                    if let Some(infix_fn) = get_infix_fn(&self.peek_token.token_type) {
                        self.next_token();
                        lhs = infix_fn(self, Box::new(lhs.clone()))?;
                    } else {
                        return Ok(lhs);
                    }
                }

                Ok(lhs)
            }
            None => Err(format!(
                "no prefix function found for {}",
                self.curr_token.literal
            )),
        }
    }

    fn parse_identifier_expression(&mut self) -> Result<Expression, ParserError> {
        Ok(Expression::Identifier(IdentifierExpression {
            name: self.curr_token.literal.clone(),
        }))
    }

    fn parse_integer_expression(&mut self) -> Result<Expression, ParserError> {
        match self.curr_token.literal.parse() {
            Ok(int) => Ok(Expression::Integer(IntegerExpression { value: int })),
            Err(_) => Err(format!(
                "expected integer, found {}",
                self.curr_token.literal
            )),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let prefix = self.curr_token.clone();
        self.next_token();
        let operand = self.parse_expression(Consts::PREFIX)?;

        Ok(Expression::Prefix(PrefixExpression {
            operator: prefix,
            operand: Box::new(operand),
        }))
    }

    fn parse_infix_expression(&mut self, lhs: Box<Expression>) -> Result<Expression, ParserError> {
        let operator = self.curr_token.clone();
        let precedence = get_precedence(&self.curr_token.token_type);
        self.next_token();
        let rhs = self.parse_expression(precedence)?;

        Ok(Expression::Infix(InfixExpression {
            operator,
            lhs,
            rhs: Box::new(rhs),
        }))
    }

    fn parse_boolean_expression(&mut self) -> Result<Expression, ParserError> {
        match self.curr_token.literal.parse() {
            Ok(boolean) => Ok(Expression::Boolean(BooleanExpression { value: boolean })),
            Err(_) => Err(format!(
                "expected boolean, found {}",
                self.curr_token.literal
            )),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();
        let expr = self.parse_expression(Consts::LOWEST)?;
        self.expect_peek(&TokenType::RPARAN)?;

        Ok(expr)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek(&TokenType::LPARAN)?;

        self.next_token();
        let expr = self.parse_expression(Consts::LOWEST)?;
        let condition = Box::new(expr);

        self.expect_peek(&TokenType::RPARAN)?;

        self.expect_peek(&TokenType::LBRACE)?;

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token.token_type == TokenType::ELSE {
            self.next_token();
            self.expect_peek(&TokenType::LBRACE)?;

            Some(self.parse_block_statement())
        } else {
            None
        };

        Ok(Expression::If(IfExpression {
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_fn_literal_expression(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek(&TokenType::LPARAN)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(&TokenType::LBRACE)?;

        let body = self.parse_block_statement();

        Ok(Expression::FnLiteral(FnLiteralExpression {
            name: "".to_string(),
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<IdentifierExpression>, ParserError> {
        let mut parameters = Vec::new();

        if self.peek_token.token_type == TokenType::RPARAN {
            self.next_token();
            return Ok(parameters);
        };

        self.next_token();

        match &self.curr_token.token_type {
            TokenType::IDENT(name) => {
                parameters.push(IdentifierExpression {
                    name: name.to_owned(),
                });
            }
            token => {
                return Err(format!(
                    "expected identifier, found {:?}",
                    token
                ))
            }
        }

        while self.peek_token.token_type == TokenType::COMMA {
            self.next_token();
            self.next_token();
            match &self.curr_token.token_type {
                TokenType::IDENT(identifier) => parameters.push(IdentifierExpression {
                    name: identifier.to_owned(),
                }),
                token => {
                    return Err(format!(
                        "expected identifier, found {:?}",
                        token
                    ))
                }
            }
        }

        self.expect_peek(&TokenType::RPARAN)?;

        Ok(parameters)
    }

    fn parse_string_literal_expression(&mut self) -> Result<Expression, ParserError> {
        Ok(Expression::String(StringExpression {
            value: self.curr_token.literal.clone(),
        }))
    }

    fn parse_call_expression(&mut self, lhs: Box<Expression>) -> Result<Expression, ParserError> {
        Ok(Expression::Call(CallExpression {
            function: lhs,
            arguments: self.parse_expression_list(TokenType::RPARAN)?,
        }))
    }

    fn parse_array_literal_expression(&mut self) -> Result<Expression, ParserError> {
        Ok(Expression::ArrayLiteral(ArrayLiteralExpression {
            elements: self.parse_expression_list(TokenType::RBRACE)?,
        }))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Result<Vec<Expression>, ParserError> {
        let mut exprs = Vec::new();

        if self.peek_token.token_type == end {
            self.next_token();
            return Ok(exprs);
        }

        self.next_token();
        if let Ok(expr) = self.parse_expression(Consts::LOWEST) {
            exprs.push(expr);
        }

        while self.peek_token.token_type == TokenType::COMMA {
            self.next_token();
            self.next_token();
            if let Ok(expr) = self.parse_expression(Consts::LOWEST) {
                exprs.push(expr);
            }
        }

        self.expect_peek(&end)?;

        Ok(exprs)
    }

    fn parse_index_expression(&mut self, lhs: Box<Expression>) -> Result<Expression, ParserError> {
        self.next_token();
        let index = self.parse_expression(Consts::LOWEST)?;
        self.expect_peek(&TokenType::RBRACE)?;

        Ok(Expression::Index(IndexExpression {
            identifier: lhs,
            index: Box::new(index),
        }))
    }

    fn parse_hash_literal_expression(&mut self) -> Result<Expression, ParserError> {
        let mut pairs = BTreeMap::new();

        while self.peek_token.token_type != TokenType::RBRACE {
            self.next_token();
            let key = self.parse_expression(Consts::LOWEST)?;

            self.expect_peek(&TokenType::COLON)?;

            self.next_token();
            let value = self.parse_expression(Consts::LOWEST)?;
            pairs.insert(key, value);

            if self.peek_token.token_type != TokenType::RBRACE {
                self.expect_peek(&TokenType::COMMA)?;
            }
        }

        self.expect_peek(&TokenType::RBRACE)?;

        Ok(Expression::HashLiteral(HashLiteralExpression { pairs }))
    }

    fn expect_peek(&mut self, expected: &TokenType) -> Result<(), ParserError> {
        match &self.peek_token {
            token if token.token_type == *expected => {
                self.next_token();
                Ok(())
            }
            _ => Err(fmt_token_error(expected, &self.peek_token.token_type)),
        }
    }
}

