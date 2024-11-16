use crate::token::Token;
use std::collections::BTreeMap;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::new()
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Statement {
    pub fn token_literal(&self) -> String {
        String::new()
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub identifier: IdentifierExpression,
    pub value: Expression,
}

impl LetStatement {
    pub fn token_literal(&self) -> String {
        self.token.literal
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct ReturnStatement {
    pub value: Expression,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct ExpressionStatement {
    pub expr: Expression,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub enum Expression {
    Identifier(IdentifierExpression),
    Integer(IntegerExpression),
    String(StringExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanExpression),
    If(IfExpression),
    FnLiteral(FnLiteralExpression),
    ArrayLiteral(ArrayLiteralExpression),
    HashLiteral(HashLiteralExpression),
    Call(CallExpression),
    Index(IndexExpression),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct IdentifierExpression {
    pub token: Token,
    pub name: String,
}

impl IdentifierExpression {
    pub fn token_literal(&self) -> String {
        self.token.literal
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct IntegerExpression {
    pub value: i32,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct StringExpression {
    pub value: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct PrefixExpression {
    pub operator: Token,
    pub operand: Box<Expression>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct InfixExpression {
    pub operator: Token,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct BooleanExpression {
    pub value: bool,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct FnLiteralExpression {
    pub name: String,
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockStatement,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct ArrayLiteralExpression {
    pub elements: Vec<Expression>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct HashLiteralExpression {
    pub pairs: BTreeMap<Expression, Expression>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct IndexExpression {
    pub identifier: Box<Expression>,
    pub index: Box<Expression>,
}

/*
fn fmt_statements(stmts: &[Statement], separator: &str) -> String {
    stmts
        .iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join(separator)
}

pub fn fmt_identifier_expressions(exprs: &[IdentifierExpression], separator: &str) -> String {
    exprs
        .iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join(separator)
}

fn fmt_expressions(exprs: &[Expression], separator: &str) -> String {
    exprs
        .iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join(separator)
}
*/
