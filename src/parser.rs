use crate::syntax::*;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while_m_n},
    character::complete::{
        alphanumeric0, anychar, char, digit1, multispace0, multispace1, one_of, space0, space1,
    },
    combinator::{complete, cut, flat_map, map, opt, peek},
    error::VerboseError,
    multi::{fold_many0, many1, separated_list},
    sequence::{pair, preceded, terminated, tuple},
    IResult,
};
//use std::result::Result;

fn statements<'a>(i: &'a str) -> IResult<&'a str, Vec<Statement>, VerboseError<&'a str>> {
    alt((
        map(tuple((char('^'), space1, expr)), |(_, _, e)| {
            vec![Statement::Ret(e)]
        }),
        map(
            tuple((expr, char('.'), space1, statements)),
            |(e, _, _, s)| {
                let mut m = Vec::new();
                m.push(Statement::E(e));
                m.extend(s);
                m
            },
        ),
        map(expr, |e| vec![Statement::E(e)]),
        |s: &'a str| {
            println!("nothing statement !!!");
            Ok((s, vec![]))
        },
    ))(i)
}

fn block<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    preceded(
        pair(char('['), multispace0),
        cut(terminated(
            map(
                tuple((block_vars, multispace0, char('|'), multispace0, statements)),
                |(vars, _, _, _, statements)| Expr::Block { vars, statements },
            ),
            char(']'),
        )),
    )(i)
}

fn block_vars<'a>(i: &'a str) -> IResult<&'a str, Vec<Ident>, VerboseError<&'a str>> {
    many1(map(tuple((char(':'), ident, space0)), |t| t.1))(i)
}
/*
parser! {
    fn block_vars[I]()(I) -> Vec<Ident>
        where [I: Stream<Item = char>]
    {
        many1((token(':'), ident()).map(|t| t.1))
    }
}
*/

fn message_expr<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    alt((keyword_expr, binary_expr, unary_expr))(i)
}
fn cascaded_message_expr<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    let next = map(
        tuple((
            opt(char(';')),
            alt((
                unary_selector,
                map(tuple((multispace0, binary_selector, unary_object)), |(_, bin_sel, expr)| {
                    Msg::Binary(bin_sel, Box::new(expr))
                }),
                map(
                    many1(preceded(multispace0, map(pair(keyword_lit, binary_object), |(id, val)| Keyword {
                        id: Ident(id),
                        val,
                    }))),
                    Msg::Kwargs,
                ),
            )),
        )),
        |t| t.1,
    );

    map(
        tuple((message_expr, many1(next))),
        |(a, b): (_, Vec<Msg>)| {
            b.iter().fold(a, |acc, msg| Expr::Message {
                receiver: Box::new(acc),
                selector: msg.clone(),
            })
        },
    )(i)
}

fn unary_object<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    primary(i)
}

fn unary_expr<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    let (i, init) = unary_object(i)?;

    fold_many0(
        pair(space1, unary_selector),
        init,
        |acc, item: (&str, Msg)| Expr::Message {
            receiver: Box::new(acc),
            selector: item.1,
        },
    )(i)
}

fn unary_selector<'a>(i: &'a str) -> IResult<&'a str, Msg, VerboseError<&'a str>> {
    map(ident, Msg::Unary)(i)
}

fn keyword_expr<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    // look ahead without consuming any input up to the selector to make sure it's a keyword one, continuing with parsing the keyword expression 
    // only if successful.  
    let (i, _) = peek(tuple((binary_object, multispace1, keyword_lit)))(i)?;
    let (i, init) = binary_object(i)?;

    fold_many0(
        many1(map(
            tuple((multispace1, keyword_lit, binary_object)),
            |(_, s, o)| Keyword {
                id: Ident(s),
                val: o,
            },
        )),
        init,
        |acc, item| Expr::Message {
            receiver: Box::new(acc),
            selector: Msg::Kwargs(item),
        },
    )(i)
}

fn binary_expr<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    // look ahead, without consuming any input, up to the selector to make sure it's a binary one, continuing with parsing the binary expression 
    // ONLY if successful. Without this, we may be parsing input as a binary expression which is not, therefore producing nodes of the wrong type. 
    let (i, _) = peek(tuple((unary_object, multispace1, binary_selector)))(i)?;
    let (i, init) = unary_object(i)?;

    fold_many0(
        tuple((multispace1, binary_selector, unary_object)),
        init,
        |acc, item| Expr::Message {
            receiver: Box::new(acc),
            selector: Msg::Binary(item.1, Box::new(item.2)),
        },
    )(i)
}

fn binary_object<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    alt((unary_object, binary_expr))(i)
}

fn expr<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    alt((
        map(tuple((ident, assignment, expr)), |t| {
            Expr::Assign(t.0, Box::new(t.2))
        }),
        cascaded_message_expr,
        message_expr,
        primary,
        map(method_p, Expr::Method),
    ))(i)
}

fn primary<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    alt((
        map(ident, Expr::Id),
        map(literal, Expr::Lit),
        block,
        preceded(pair(char('('), space0), terminated(expr, char(')'))),
    ))(i)
}

fn assignment<'a>(i: &'a str) -> IResult<&'a str, &str, VerboseError<&'a str>> {
    map(
        tuple((multispace0, tag("<-"), multispace0)),
        |(s1, _, s2)| s2,
    )(i)
}

fn array<'a>(i: &'a str) -> IResult<&'a str, Literal, VerboseError<&'a str>> {
    map(
        preceded(
            char('('),
            cut(terminated(
                separated_list(
                    char(' '),
                    alt((map(number, Literal::Number), symbol, string, sm_char, array)),
                ),
                char(')'),
            )),
        ),
        Literal::Array,
    )(i)
}

fn literal<'a>(i: &'a str) -> IResult<&'a str, Literal, VerboseError<&'a str>> {
    alt((
        map(tuple((space0, number)), |(_, n)| Literal::Number(n)),
        sm_char,
        string,
        preceded(char('#'), alt((array, symbol))),
    ))(i)
}

/// Parse a Smalltalk character.
fn sm_char<'a>(i: &'a str) -> IResult<&'a str, Literal, VerboseError<&'a str>> {
    map(preceded(tag("$"), anychar), |a_char| Literal::Char(a_char))(i)
}

fn number<'a>(i: &'a str) -> IResult<&'a str, Num, VerboseError<&'a str>> {
    map(
        tuple((
            opt(map(terminated(digit1, tag("r")), |digit_str: &str| {
                digit_str.parse::<u8>().unwrap()
            })),
            alt((digit1, take_while_m_n(1, 3, char::is_uppercase))),
            opt(preceded(
                tag("."),
                alt((digit1, take_while_m_n(1, 3, char::is_uppercase))),
            )),
            opt(map(preceded(tag("e"), digit1), |digit_str: &str| {
                digit_str.parse::<u32>().unwrap()
            })),
        )),
        |(r, i, m, e)| Num {
            radix: r,
            integer: String::from(i),
            mantissa: m.map(String::from),
            exponent: e,
        },
    )(i)
}

fn string<'a>(i: &'a str) -> IResult<&'a str, Literal, VerboseError<&'a str>> {
    map(
        preceded(
            char('\''),
            cut(terminated(
                take_while(|x| char::is_alphabetic(x) || char::is_whitespace(x)),
                char('\''),
            )),
        ),
        |s| Literal::Str(String::from(s)),
    )(i)
}

fn symbol<'a>(i: &'a str) -> IResult<&'a str, Literal, VerboseError<&'a str>> {
    alt((
        map(ident, |Ident(i)| Literal::Symbol(i)),
        map(binary_selector, Literal::Symbol),
        map(many1(keyword_lit), |kws| Literal::Symbol(kws.join(""))),
    ))(i)
}

fn keyword_lit<'a>(i: &'a str) -> IResult<&'a str, String, VerboseError<&'a str>> {
    map(tuple((ident, tag(":"), space0)), |(Ident(i), _, _)| {
        format!("{}:", i)
    })(i)
}

fn ident<'a>(i: &'a str) -> IResult<&'a str, Ident, VerboseError<&'a str>> {
    map(
        tuple((take_while_m_n(1, 1, char::is_alphabetic), alphanumeric0)),
        |t| Ident(format!("{}{}", t.0, t.1)),
    )(i)
}

fn binary_selector<'a>(i: &'a str) -> IResult<&'a str, String, VerboseError<&'a str>> {
    map(
        alt((
            tuple((space0, special_char, opt(special_char), space0)),
            map(tag("-"), |t: &str| {
                ("", t.chars().nth(0).unwrap(), None, "")
            }),
        )),
        |(_, c, mc, _)| match mc {
            Some(x) => format!("{}{}", c, x),
            None => format!("{}", c),
        },
    )(i)
}

fn special_char<'a>(i: &'a str) -> IResult<&'a str, char, VerboseError<&'a str>> {
    one_of("+/\\*~<>=@%|&?!")(i)
}

fn message_pattern<'a>(i: &'a str) -> IResult<&'a str, MsgPat, VerboseError<&'a str>> {
    let kwargs = map(
        many1(map(pair(keyword_lit, ident), |(k, var)| KeyPat {
            keyword: Ident(k),
            var,
        })),
        MsgPat::Kwargs,
    );

    let bin = map(pair(binary_selector, ident), |(a, b)| {
        MsgPat::Bin(Ident(a), b)
    });

    alt((kwargs, bin, map(ident, MsgPat::Unary)))(i)
}

fn temporaries<'a>(i: &'a str) -> IResult<&'a str, Vec<Ident>, VerboseError<&'a str>> {
    preceded(
        pair(char('|'), space0),
        cut(terminated(
            separated_list(char(' '), ident),
            pair(space0, char('|')),
        )),
    )(i)
}

fn method_p<'a>(i: &'a str) -> IResult<&'a str, Method, VerboseError<&'a str>> {
    map(
        tuple((
            message_pattern,
            opt(preceded(space1, temporaries)),
            opt(preceded(space1, statements)),
        )),
        |(sig, temps, stmts)| Method { sig, temps, stmts },
    )(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    // Extracts a string that does not contain whitespace, i.e. comma or tab.
    #[test]
    fn test_bare_number() {
        let res = number("10");
        let ans = Num::int_from_str("10");
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_exponent() {
        let res = number("10e3");
        let ans = Num {
            integer: String::from("10"),
            exponent: Some(3),
            mantissa: None,
            radix: None,
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_full_number() {
        let res = number("10r10.5e3");
        let ans = Num {
            integer: String::from("10"),
            exponent: Some(3),
            mantissa: Some(String::from("5")),
            radix: Some(10),
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_float() {
        let res = number("123.456");
        let ans = Num {
            integer: String::from("123"),
            exponent: None,
            mantissa: Some(String::from("456")),
            radix: None,
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_radix() {
        let res = number("16rAC.DCe10");
        let ans = Num {
            integer: String::from("AC"),
            exponent: Some(10),
            mantissa: Some(String::from("DC")),
            radix: Some(16),
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_char() {
        let res = sm_char("$a");
        let ans = Literal::Char('a');
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_string() {
        let res = string("'hello world'");
        let ans = Literal::Str(String::from("hello world"));
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_ident() {
        let res = ident("index");
        let ans = mk_ident("index");
        assert_eq!(res, Ok(("", ans)))
    }
    #[test]
    fn test_symbol() {
        let res = literal("#foobar123");
        let ans = Literal::Symbol(String::from("foobar123"));
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_literal() {
        let res = literal("#('hello' 123 world)");
        let ans = Literal::Array(vec![
            Literal::Str(String::from("hello")),
            Literal::Number(Num::int_from_str("123")),
            Literal::Symbol(String::from("world")),
        ]);
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_single_assignment() {
        let res = expr("foo <- bar");
        let ans = Expr::Assign(mk_ident("foo"), Box::new(mk_ident_expr("bar")));
        assert_eq!(res, Ok(("", ans)))
    }

    #[test]
    fn test_expr_assigment() {
        let res = expr("foo <- 'hello world'");
        let ans = Expr::Assign(
            mk_ident("foo"),
            Box::new(Expr::Lit(Literal::Str(String::from("hello world")))),
        );
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_assign_number() {
        let res = expr("foo <- 3r2e3");
        let ans = Expr::Assign(
            mk_ident("foo"),
            Box::new(Expr::Lit(Literal::Number(Num {
                radix: Some(3),
                integer: String::from("2"),
                mantissa: None,
                exponent: Some(3),
            }))),
        );
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_multiple_assignment() {
        let res = expr("foo <- bar <- 'hello world'");
        let ans = Expr::Assign(
            mk_ident("foo"),
            Box::new(Expr::Assign(
                mk_ident("bar"),
                Box::new(Expr::Lit(Literal::Str(String::from("hello world")))),
            )),
        );
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_unary_message_expr() {
        let res = unary_expr("theta sin");
        let ans = Expr::Message {
            receiver: Box::new(mk_ident_expr("theta")),
            selector: Msg::Unary(mk_ident("sin")),
        };
        //println!("This is it!!! {:?}", res.is_err());
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_binary_expr_num() {
        let res = expr("3 + 2");
        let ans = Expr::Message {
            receiver: Box::new(Expr::Lit(Literal::Number(Num::int_from_str("3")))),
            selector: Msg::Binary(
                String::from("+"),
                Box::new(Expr::Lit(Literal::Number(Num::int_from_str("2")))),
            ),
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_binary_expr() {
        let res = expr("foo + 2");
        let ans = Expr::Message {
            receiver: Box::new(mk_ident_expr("foo")),
            selector: Msg::Binary(
                String::from("+"),
                Box::new(Expr::Lit(Literal::Number(Num::int_from_str("2")))),
            ),
        };
        assert_eq!(res, Ok(("", ans)));
    }
    #[test]
    fn test_keyword_message() {
        let res = expr("a b: 2");
        let ans = Expr::Message {
            receiver: Box::new(mk_ident_expr("a")),
            selector: Msg::Kwargs(vec![Keyword {
                id: mk_ident("b:"),
                val: Expr::Lit(Literal::Number(Num::int_from_str("2"))),
            }]),
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_keyword_messages() {
        let res = expr("a b: 2 c: 3");
        let ans = Expr::Message {
            receiver: Box::new(mk_ident_expr("a")),
            selector: Msg::Kwargs(vec![
                Keyword {
                    id: mk_ident("b:"),
                    val: Expr::Lit(Literal::Number(Num::int_from_str("2"))),
                },
                Keyword {
                    id: mk_ident("c:"),
                    val: Expr::Lit(Literal::Number(Num::int_from_str("3"))),
                },
            ]),
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_many_unary_messages() {
        let res = expr("theta sin round");
        let ans = Expr::Message {
            receiver: Box::new(Expr::Message {
                receiver: Box::new(mk_ident_expr("theta")),
                selector: Msg::Unary(mk_ident("sin")),
            }),
            selector: Msg::Unary(mk_ident("round")),
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_empty_statements() {
        let res = statements("");
        let ans = vec![];
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_empty_expr() {
        let res = expr("");
        assert!(res.is_err());
    }

    #[test]
    fn test_empty_ident() {
        assert!(ident("").is_err());
    }

    #[test]
    fn test_empty_primary() {
        assert!(primary("").is_err());
    }

    #[test]
    fn test_empty_message_expr() {
        assert!(message_expr("").is_err());
    }

    #[test]
    fn test_empty_keyword_expr() {
        assert!(keyword_expr("").is_err());
    }

    #[test]
    fn test_empty_binary_object() {
        assert!(binary_object("").is_err());
    }

    #[test]
    fn test_empty_unary_object() {
        assert!(unary_object("").is_err());
    }

    #[test]
    fn test_empty_cascaded_message_expr() {
        assert!(cascaded_message_expr("").is_err());
    }

    #[test]
    fn test_expr_statement() {
        let res = statements("what");
        let ans = vec![Statement::E(mk_ident_expr("what"))];
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_return_statement() {
        let res = statements("^ 'foo'");
        let ans = vec![Statement::Ret(Expr::Lit(Literal::Str(String::from("foo"))))];
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_many_statements() {
        let res = statements("foo <- bar. ^ foo");
        let ans = vec![
            Statement::E(Expr::Assign(
                mk_ident("foo"),
                Box::new(mk_ident_expr("bar")),
            )),
            Statement::Ret(mk_ident_expr("foo")),
        ];
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_message_pattern_unary() {
        let res = message_pattern("hello");
        let ans = MsgPat::Unary(mk_ident("hello"));
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_message_pattern_binary() {
        let res = message_pattern("+ hello");
        let ans = MsgPat::Bin(mk_ident("+"), mk_ident("hello"));
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_mssage_pattern_kwargs() {
        let res = message_pattern("foo: bar");
        let ans = MsgPat::Kwargs(vec![KeyPat {
            keyword: mk_ident("foo:"),
            var: mk_ident("bar"),
        }]);
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_method() {
        let res = method_p("foo ^ bar");
        let ans = Method {
            sig: MsgPat::Unary(mk_ident("foo")),
            temps: None,
            stmts: Some(vec![Statement::Ret(mk_ident_expr("bar"))]),
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_method_temps() {
        let res = method_p("foo |asdf| ^ bar");
        let ans = Method {
            sig: MsgPat::Unary(mk_ident("foo")),
            temps: Some(vec![mk_ident("asdf")]),
            stmts: Some(vec![Statement::Ret(mk_ident_expr("bar"))]),
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_method_bare_ret_kwargs() {
        let res = method_p("foo: asdf bar");
        let ans = Method {
            sig: MsgPat::Kwargs(vec![KeyPat {
                keyword: mk_ident("foo:"),
                var: mk_ident("asdf"),
            }]),
            temps: None,
            stmts: Some(vec![Statement::E(mk_ident_expr("bar"))]),
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_method_bare_ret() {
        let res = method_p("foo bar");
        let ans = Method {
            sig: MsgPat::Unary(mk_ident("foo")),
            temps: None,
            stmts: Some(vec![Statement::E(mk_ident_expr("bar"))]),
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_method_kwargs() {
        let res = method_p("foo: asdf ^ bar");
        let ans = Method {
            sig: MsgPat::Kwargs(vec![KeyPat {
                keyword: mk_ident("foo:"),
                var: mk_ident("asdf"),
            }]),
            temps: None,
            stmts: Some(vec![Statement::Ret(mk_ident_expr("bar"))]),
        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_temporaries_empty() {
        let res = temporaries("");
        assert!(res.is_err());
    }

    #[test]
    fn test_temporaries() {
        let res = temporaries("| foo |");
        let ans = vec![mk_ident("foo")];
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_block() {
        let res = block("[:x :y| x + 1. x + y]");
        let ans = Expr::Block {
            vars: vec![Ident("x".to_string()), Ident("y".to_string())],
            statements: vec![
                Statement::E(Expr::Message {
                    receiver: Box::new(Expr::Id(Ident("x".to_string()))),
                    selector: Msg::Binary(
                        String::from("+"),
                        Box::new(Expr::Lit(Literal::Number(Num::int_from_str("1")))),
                    ),
                }),
                Statement::E(Expr::Message {
                    receiver: Box::new(Expr::Id(Ident("x".to_string()))),
                    selector: Msg::Binary(
                        String::from("+"),
                        Box::new(Expr::Id(Ident("y".to_string()))),
                    ),
                }),
            ],
        };

        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_cascaded_message_expr() {
        let res = cascaded_message_expr("a b: 'b'; c: 'c'");
        let ans = Expr::Message {
                            receiver: Box::new(
                                Expr::Message {
                                    receiver: Box::new(Expr::Id(Ident("a".to_string()))),
                                    selector: Msg::Kwargs(vec![
                                            Keyword { id: Ident("b:".to_string()), val: Expr::Lit(Literal::Str("b".to_string())) }
                                        ])
                                }),
                            selector: Msg::Kwargs(vec![
                                Keyword { id: Ident("c:".to_string()), val: Expr::Lit(Literal::Str("c".to_string()))
                            }])
                        };
        assert_eq!(res, Ok(("", ans)));
    }

    #[test]
    fn test_rectangle_constructor() {
        let res = expr(
            "Rectangle
    origin: (Point x:0 y:10)
    extent: (Point x:5 y:15)",
        );
        let ans = Expr::Message {
            receiver: Box::new(mk_ident_expr("Rectangle")),
            selector: Msg::Kwargs(vec![
                Keyword {
                    id: mk_ident("origin:"),
                    val: Expr::Message {
                        receiver: Box::new(mk_ident_expr("Point")),
                        selector: Msg::Kwargs(vec![
                            Keyword {
                                id: mk_ident("x:"),
                                val: Num::int_from_str("0").to_expr(),
                            },
                            Keyword {
                                id: mk_ident("y:"),
                                val: Num::int_from_str("10").to_expr(),
                            },
                        ]),
                    },
                },
                Keyword {
                    id: mk_ident("extent:"),
                    val: Expr::Message {
                        receiver: Box::new(mk_ident_expr("Point")),
                        selector: Msg::Kwargs(vec![
                            Keyword {
                                id: mk_ident("x:"),
                                val: Num::int_from_str("5").to_expr(),
                            },
                            Keyword {
                                id: mk_ident("y:"),
                                val: Num::int_from_str("15").to_expr(),
                            },
                        ]),
                    },
                },
            ]),
        };

        assert_eq!(res, Ok(("", ans)));
    }
}
