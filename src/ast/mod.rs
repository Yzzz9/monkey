use crate::token::Token;
use std::any::Any;

pub trait Node {
    fn token_literal(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}
pub trait Statement: Node {
    fn statement_node(&self);
}
pub trait Expression: Node {
    fn expression_node(&self);
}

// Identifier struct
#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}
impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for Identifier {
    fn expression_node(&self) {}
}

// LetStatement struct
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}
impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Statement for LetStatement {
    fn statement_node(&self) {}
}

// Program struct
pub struct Program {
    pub statements: Vec<Box<dyn Node>>,
}
impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            return (*self.statements[0].token_literal()).to_string();
        }
        "".to_string()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Statement for Program {
    fn statement_node(&self) {}
}
impl Program {
    pub fn new() -> Self {
        Program { statements: vec![] }
    }
}
