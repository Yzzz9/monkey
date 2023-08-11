use crate::token::Token;
use std::any::Any;

pub trait Node {
    fn token_literal(&self) -> String;
    fn as_any(&self) -> &dyn Any;
    fn string(&self) -> String;
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
    fn string(&self) -> String {
        self.value.clone()
    }
}
impl Expression for Identifier {
    fn expression_node(&self) {}
}

// IntegerLiteral struct
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}
impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn string(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}

// PrefixExpression struct
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}
impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn string(&self) -> String {
        format!(
            "({}{})",
            self.operator,
            self.right.as_ref().unwrap().string()
        )
    }
}
impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

// InfixExpression struct
pub struct InfixExpression {
    pub token: Token,
    pub left: Option<Box<dyn Expression>>,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}
impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.as_ref().unwrap().string(),
            self.operator,
            self.right.as_ref().unwrap().string()
        )
    }
}
impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

// LetStatement struct
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}
impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn string(&self) -> String {
        format!(
            "{} {} = {};",
            self.token_literal(),
            self.name.string(),
            self.value.as_ref().unwrap().string()
        )
    }
}
impl Statement for LetStatement {
    fn statement_node(&self) {}
}

// ReturnStatement struct
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>,
}
impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn string(&self) -> String {
        format!(
            "{} {};",
            self.token_literal(),
            self.return_value.as_ref().unwrap().string()
        )
    }
}
impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

// ExpressionStatement struct
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}
impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn string(&self) -> String {
        format!("{}", self.expression.as_ref().unwrap().string())
    }
}
impl Statement for ExpressionStatement {
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
    fn string(&self) -> String {
        let mut return_string = String::new();
        for stmt in self.statements.iter() {
            return_string.push_str(&format!("{}", stmt.string()));
        }
        return_string
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
