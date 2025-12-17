use std::ops::Range;

use super::lexer::{Lexer, Token, TokenSpan};

#[derive(Debug, Clone, PartialEq)]
pub enum Sexp {
    Symbol(String),
    Number(f64),
    String(String),
    Bool(bool),
    List(Vec<Sexp>),                // ()
    Vector(Vec<Sexp>),              // []
    Map(Vec<(Sexp, Sexp)>),         // {}
    Tagged(String, Vec<Sexp>),      // <tag attrs children>
    Colon,                          // : (for type annotations)
    Arrow,                          // -> (for function types)
}

#[derive(Debug, Clone)]
pub struct SexpSpan {
    pub sexp: Sexp,
    pub span: Range<usize>,
}

pub struct Parser {
    tokens: Vec<TokenSpan>,
    pos: usize,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        Self { tokens, pos: 0 }
    }

    fn current(&self) -> &Token {
        if self.pos < self.tokens.len() {
            &self.tokens[self.pos].token
        } else {
            &Token::Eof
        }
    }

    fn advance(&mut self) -> Token {
        let token = self.current().clone();
        self.pos += 1;
        token
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        let token = self.current();
        if std::mem::discriminant(token) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected {:?}, found {:?}", expected, token))
        }
    }

    pub fn parse(&mut self) -> Result<Vec<SexpSpan>, String> {
        let mut sexps = Vec::new();
        while !matches!(self.current(), Token::Eof) {
            sexps.push(self.parse_sexp()?);
        }
        Ok(sexps)
    }

    fn parse_sexp(&mut self) -> Result<SexpSpan, String> {
        let start = if self.pos < self.tokens.len() {
            self.tokens[self.pos].span.start
        } else {
            0
        };

        let sexp = match self.current().clone() {
            Token::Number(n) => {
                self.advance();
                Sexp::Number(n)
            }
            Token::String(s) => {
                self.advance();
                Sexp::String(s)
            }
            Token::Bool(b) => {
                self.advance();
                Sexp::Bool(b)
            }
            Token::Symbol(s) => {
                self.advance();
                Sexp::Symbol(s)
            }
            Token::Colon => {
                self.advance();
                Sexp::Colon
            }
            Token::Arrow => {
                self.advance();
                Sexp::Arrow
            }
            Token::LParen => self.parse_list()?,
            Token::LBracket => self.parse_vector()?,
            Token::LBrace => self.parse_map()?,
            Token::LAngle => self.parse_tagged()?,
            token => {
                return Err(format!("Unexpected token: {:?}", token));
            }
        };

        let end = if self.pos > 0 && self.pos <= self.tokens.len() {
            self.tokens[self.pos - 1].span.end
        } else {
            start
        };

        Ok(SexpSpan {
            sexp,
            span: start..end,
        })
    }

    fn parse_list(&mut self) -> Result<Sexp, String> {
        self.expect(Token::LParen)?;
        let mut elements = Vec::new();

        while !matches!(self.current(), Token::RParen | Token::Eof) {
            elements.push(self.parse_sexp()?.sexp);
        }

        self.expect(Token::RParen)?;
        Ok(Sexp::List(elements))
    }

    fn parse_vector(&mut self) -> Result<Sexp, String> {
        self.expect(Token::LBracket)?;
        let mut elements = Vec::new();

        while !matches!(self.current(), Token::RBracket | Token::Eof) {
            elements.push(self.parse_sexp()?.sexp);
        }

        self.expect(Token::RBracket)?;
        Ok(Sexp::Vector(elements))
    }

    fn parse_map(&mut self) -> Result<Sexp, String> {
        self.expect(Token::LBrace)?;
        let mut pairs = Vec::new();

        while !matches!(self.current(), Token::RBrace | Token::Eof) {
            let key = self.parse_sexp()?.sexp;

            // Handle optional colon after key
            if matches!(self.current(), Token::Colon) {
                self.advance();
            }

            let value = self.parse_sexp()?.sexp;
            pairs.push((key, value));
        }

        self.expect(Token::RBrace)?;
        Ok(Sexp::Map(pairs))
    }

    fn parse_tagged(&mut self) -> Result<Sexp, String> {
        self.expect(Token::LAngle)?;

        // Parse tag name
        let tag = match self.current().clone() {
            Token::Symbol(s) => {
                self.advance();
                s
            }
            token => {
                return Err(format!("Expected tag name, found {:?}", token));
            }
        };

        // Parse attributes and children
        let mut children = Vec::new();

        while !matches!(self.current(), Token::RAngle | Token::Eof) {
            children.push(self.parse_sexp()?.sexp);
        }

        self.expect(Token::RAngle)?;
        Ok(Sexp::Tagged(tag, children))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_number() {
        let mut parser = Parser::new("42");
        let sexps = parser.parse().unwrap();
        assert_eq!(sexps.len(), 1);
        assert_eq!(sexps[0].sexp, Sexp::Number(42.0));
    }

    #[test]
    fn test_parse_string() {
        let mut parser = Parser::new(r#""hello""#);
        let sexps = parser.parse().unwrap();
        assert_eq!(sexps.len(), 1);
        assert_eq!(sexps[0].sexp, Sexp::String("hello".to_string()));
    }

    #[test]
    fn test_parse_symbol() {
        let mut parser = Parser::new("foo");
        let sexps = parser.parse().unwrap();
        assert_eq!(sexps.len(), 1);
        assert_eq!(sexps[0].sexp, Sexp::Symbol("foo".to_string()));
    }

    #[test]
    fn test_parse_simple_list() {
        let mut parser = Parser::new("(+ 1 2)");
        let sexps = parser.parse().unwrap();
        assert_eq!(sexps.len(), 1);
        assert_eq!(
            sexps[0].sexp,
            Sexp::List(vec![
                Sexp::Symbol("+".to_string()),
                Sexp::Number(1.0),
                Sexp::Number(2.0),
            ])
        );
    }

    #[test]
    fn test_parse_nested_list() {
        let mut parser = Parser::new("(+ (* 2 3) 4)");
        let sexps = parser.parse().unwrap();
        assert_eq!(sexps.len(), 1);
        assert_eq!(
            sexps[0].sexp,
            Sexp::List(vec![
                Sexp::Symbol("+".to_string()),
                Sexp::List(vec![
                    Sexp::Symbol("*".to_string()),
                    Sexp::Number(2.0),
                    Sexp::Number(3.0),
                ]),
                Sexp::Number(4.0),
            ])
        );
    }

    #[test]
    fn test_parse_vector() {
        let mut parser = Parser::new("[1 2 3]");
        let sexps = parser.parse().unwrap();
        assert_eq!(sexps.len(), 1);
        assert_eq!(
            sexps[0].sexp,
            Sexp::Vector(vec![Sexp::Number(1.0), Sexp::Number(2.0), Sexp::Number(3.0)])
        );
    }

    #[test]
    fn test_parse_map() {
        let mut parser = Parser::new("{x 10 y 20}");
        let sexps = parser.parse().unwrap();
        assert_eq!(sexps.len(), 1);
        assert_eq!(
            sexps[0].sexp,
            Sexp::Map(vec![
                (Sexp::Symbol("x".to_string()), Sexp::Number(10.0)),
                (Sexp::Symbol("y".to_string()), Sexp::Number(20.0)),
            ])
        );
    }

    #[test]
    fn test_parse_map_with_colon() {
        let mut parser = Parser::new("{x: 10 y: 20}");
        let sexps = parser.parse().unwrap();
        assert_eq!(sexps.len(), 1);
        assert_eq!(
            sexps[0].sexp,
            Sexp::Map(vec![
                (Sexp::Symbol("x".to_string()), Sexp::Number(10.0)),
                (Sexp::Symbol("y".to_string()), Sexp::Number(20.0)),
            ])
        );
    }

    // TODO: Re-implement HTML tag syntax with a different approach
    // (e.g., special keyword or function-based syntax)
    // Currently < and > are treated as comparison operators
    /*
    #[test]
    fn test_parse_tagged() {
        let mut parser = Parser::new("<div>");
        let sexps = parser.parse().unwrap();
        assert_eq!(sexps.len(), 1);
        assert_eq!(sexps[0].sexp, Sexp::Tagged("div".to_string(), vec![]));
    }

    #[test]
    fn test_parse_tagged_with_attrs_and_children() {
        let mut parser = Parser::new(r#"<div {class: "container"} "Hello">"#);
        let sexps = parser.parse().unwrap();
        assert_eq!(sexps.len(), 1);
        assert_eq!(
            sexps[0].sexp,
            Sexp::Tagged(
                "div".to_string(),
                vec![
                    Sexp::Map(vec![(
                        Sexp::Symbol("class".to_string()),
                        Sexp::String("container".to_string())
                    )]),
                    Sexp::String("Hello".to_string()),
                ]
            )
        );
    }
    */
}
