pub mod ast {
    pub trait Node {
        pub fn token_literal(&self) -> String;
    }
    pub trait Statement {
        pub fn statement_node(&self);
    }
    pub trait Expression {
        pub fn expression_node(&self);
    }

    struct Identifier {
        token: Token,
        value: String,
    }
    impl Node for Identifier {
        fn token_literal(&self) -> String {
            self.token.literal
        }
    }
    impl Expression for Identifier {
        fn expression_node(&self) {}
    }

    struct LetStatement {
        token: Token,
        name: &Identifier,
        value: Expression,
    }
    impl Node for LetStatement {
        fn token_literal(&self) -> String {
            self.token.literal
        }
    }
    impl Statement for LetStatement {
        fn statement_node(&self) {}
    }
}
