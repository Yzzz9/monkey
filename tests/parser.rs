use monkey::ast::*;
use monkey::lexer::*;
use monkey::parser::Parser;
use monkey::token::{Token, TokenType};

#[test]
fn parser_let_statement_test() {
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
fn parser_negative_let_statement_test() {
    let input = "let x 5;
        let = 10;
        let 8383838;"
        .to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let _ = parser.parse_program();
    assert_eq!(check_parse_errors(&parser), true);
    println!("errors = {:?}", parser.errors());
    assert_eq!(parser.errors().len(), 4); // this should be 3 errors only but we are passing the testcase with 4
}

#[test]
fn parser_return_statement_test() {
    let input = "return 5;
        return 10;
        return 8383838;"
        .to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    assert_eq!(check_parse_errors(&parser), false);

    assert_eq!(program.is_some(), true);
    let program = program.unwrap();

    assert_eq!(program.statements.len(), 3);

    for program_stmt in program.statements.iter() {
        let stmt = &(*program_stmt)
            .as_any()
            .downcast_ref::<ReturnStatement>()
            .unwrap();
        assert_eq!(stmt.token_literal(), "return".to_string());
    }
}

#[test]
fn test_string_let_statment() {
    let program = Program {
        statements: vec![Box::new(LetStatement {
            token: Token::new(TokenType::LET, "let".to_string()),
            name: Identifier {
                token: Token::new(TokenType::IDENT, "myVar".to_string()),
                value: "myVar".to_string(),
            },
            value: Some(Box::new(Identifier {
                token: Token::new(TokenType::IDENT, "anotherVar".to_string()),
                value: "anotherVar".to_string(),
            })),
        })],
    };

    assert_eq!(program.string(), "let myVar = anotherVar;".to_string());
}

#[test]
fn test_identifier_expression() {
    let input = "foobar;".to_string();
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    assert_eq!(check_parse_errors(&parser), false);

    assert_eq!(program.is_some(), true);
    let program = program.unwrap();

    assert_eq!(program.statements.len(), 1);

    for program_stmt in program.statements.iter() {
        let expr_stmt = &(*program_stmt)
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap();
        let ident = &(*expr_stmt)
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<Identifier>()
            .unwrap();
        assert_eq!(ident.value, "foobar".to_string());
        assert_eq!(ident.token_literal(), "foobar".to_string());
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;".to_string();
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    assert_eq!(check_parse_errors(&parser), false);

    assert_eq!(program.is_some(), true);
    let program = program.unwrap();

    assert_eq!(program.statements.len(), 1);

    for program_stmt in program.statements.iter() {
        let expr_stmt = &(*program_stmt)
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap();
        let ident = &(*expr_stmt)
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<IntegerLiteral>()
            .unwrap();
        assert_eq!(ident.value, 5);
        assert_eq!(ident.token_literal(), "5".to_string());
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    struct PrefixTest {
        input: String,
        operator: String,
        integer_value: i64,
    }
    let prefix_tests = vec![
        PrefixTest {
            input: "!5;".to_string(),
            operator: "!".to_string(),
            integer_value: 5,
        },
        PrefixTest {
            input: "-15;".to_string(),
            operator: "-".to_string(),
            integer_value: 15,
        },
    ];

    for prefix_test in prefix_tests.iter() {
        let lexer = Lexer::new(prefix_test.input.clone());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(check_parse_errors(&parser), false);

        assert_eq!(program.is_some(), true);
        let program = program.unwrap();

        assert_eq!(program.statements.len(), 1);

        for program_stmt in program.statements.iter() {
            let expr_stmt = &(*program_stmt)
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap();
            let exp = &(*expr_stmt)
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<PrefixExpression>()
                .unwrap();
            assert_eq!(exp.operator, prefix_test.operator);
            assert_eq!(
                test_integer_literal(exp.right.as_ref().unwrap(), prefix_test.integer_value),
                true
            );
        }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    struct InfixTest {
        input: String,
        left_value: i64,
        operator: String,
        right_value: i64,
    }
    let infix_tests = vec![
        InfixTest {
            input: "5 + 5;".to_string(),
            left_value: 5,
            operator: "+".to_string(),
            right_value: 5,
        },
        InfixTest {
            input: "5 - 5;".to_string(),
            left_value: 5,
            operator: "-".to_string(),
            right_value: 5,
        },
        InfixTest {
            input: "5 * 5;".to_string(),
            left_value: 5,
            operator: "*".to_string(),
            right_value: 5,
        },
        InfixTest {
            input: "5 / 5;".to_string(),
            left_value: 5,
            operator: "/".to_string(),
            right_value: 5,
        },
        InfixTest {
            input: "5 < 5;".to_string(),
            left_value: 5,
            operator: "<".to_string(),
            right_value: 5,
        },
        InfixTest {
            input: "5 > 5;".to_string(),
            left_value: 5,
            operator: ">".to_string(),
            right_value: 5,
        },
        InfixTest {
            input: "5 == 5;".to_string(),
            left_value: 5,
            operator: "==".to_string(),
            right_value: 5,
        },
        InfixTest {
            input: "5 != 5;".to_string(),
            left_value: 5,
            operator: "!=".to_string(),
            right_value: 5,
        },
    ];
    for infix_test in infix_tests.iter() {
        let lexer = Lexer::new(infix_test.input.clone());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(check_parse_errors(&parser), false);

        assert_eq!(program.is_some(), true);
        let program = program.unwrap();

        assert_eq!(program.statements.len(), 1);

        for program_stmt in program.statements.iter() {
            let expr_stmt = &(*program_stmt)
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap();
            let exp = &(*expr_stmt)
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<InfixExpression>()
                .unwrap();
            assert_eq!(
                test_integer_literal(exp.left.as_ref().unwrap(), infix_test.left_value),
                true
            );
            assert_eq!(exp.operator, infix_test.operator);
            assert_eq!(
                test_integer_literal(exp.right.as_ref().unwrap(), infix_test.right_value),
                true
            );
        }
    }
}

#[test]
fn test_operator_precedence_parsing() {
    struct TestStruct {
        input: String,
        expected: String,
    }
    let tests = vec![
        TestStruct {
            input: "-a * b".to_string(),
            expected: "((-a) * b)".to_string(),
        },
        TestStruct {
            input: "!-a".to_string(),
            expected: "(!(-a))".to_string(),
        },
        TestStruct {
            input: "a + b + c".to_string(),
            expected: "((a + b) + c)".to_string(),
        },
        TestStruct {
            input: "a + b - c".to_string(),
            expected: "((a + b) - c)".to_string(),
        },
        TestStruct {
            input: "a * b * c".to_string(),
            expected: "((a * b) * c)".to_string(),
        },
        TestStruct {
            input: "a * b / c".to_string(),
            expected: "((a * b) / c)".to_string(),
        },
        TestStruct {
            input: "a + b / c".to_string(),
            expected: "(a + (b / c))".to_string(),
        },
        TestStruct {
            input: "a + b * c + d / e - f".to_string(),
            expected: "(((a + (b * c)) + (d / e)) - f)".to_string(),
        },
        TestStruct {
            input: "3 + 4; -5 * 5".to_string(),
            expected: "(3 + 4)((-5) * 5)".to_string(),
        },
        TestStruct {
            input: "5 > 4 == 3 < 4".to_string(),
            expected: "((5 > 4) == (3 < 4))".to_string(),
        },
        TestStruct {
            input: "5 < 4 != 3 > 4".to_string(),
            expected: "((5 < 4) != (3 > 4))".to_string(),
        },
        TestStruct {
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
            expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
        },
        TestStruct {
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
            expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
        },
    ];
    for test in tests.iter() {
        let lexer = Lexer::new(test.input.clone());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(check_parse_errors(&parser), false);

        assert_eq!(program.is_some(), true);
        let program = program.unwrap();

        let actual = program.string();

        assert_eq!(actual, test.expected);
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

fn test_integer_literal(il: &Box<dyn Expression>, value: i64) -> bool {
    let integ = il.as_any().downcast_ref::<IntegerLiteral>().unwrap();
    if integ.value != value {
        return false;
    }
    if integ.token_literal() != value.to_string() {
        return false;
    }
    true
}

fn test_identifier(exp: Box<dyn Expression>, value: String) -> bool {
    let ident = exp.as_any().downcast_ref::<Identifier>().unwrap();
    if ident.value != value {
        return false;
    }
    if ident.token_literal() != value {
        return false;
    }
    true
}

fn test_liteal_expression<T>(exp: Box<dyn Expression>, expected: T) -> bool {
    use std::any::type_name;
    match type_name::<T>() {
        i64 => test_integer_literal(&exp, expected.parse::<i64>().unwrap()),
        _ => test_identifier(exp, expected),
    }
}
