use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Error)]
#[error("{message} at {position}")]
pub struct ParseError {
    pub message: String,
    pub position: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Number(f64),
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    EOF,
}

pub fn transpile(input: &str) -> Result<String, ParseError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser { tokens, pos: 0 };
    let expr = parser.parse_expression(0)?;
    parser.expect(Token::EOF)?;
    Ok(render_js(&expr, 0))
}

fn tokenize(input: &str) -> Result<Vec<Token>, ParseError> {
    let mut chars = input.chars().peekable();
    let mut tokens = Vec::new();
    let mut idx = 0;

    while let Some(&ch) = chars.peek() {
        match ch {
            '0'..='9' | '.' => {
                let start = idx;
                let mut buf = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() || c == '.' {
                        buf.push(c);
                        chars.next();
                        idx += 1;
                    } else {
                        break;
                    }
                }
                let value: f64 = buf.parse().map_err(|_| ParseError {
                    message: "invalid number".into(),
                    position: start,
                })?;
                tokens.push(Token::Number(value));
            }
            '+' => {
                chars.next();
                idx += 1;
                tokens.push(Token::Plus);
            }
            '-' => {
                chars.next();
                idx += 1;
                tokens.push(Token::Minus);
            }
            '*' => {
                chars.next();
                idx += 1;
                tokens.push(Token::Star);
            }
            '/' => {
                chars.next();
                idx += 1;
                tokens.push(Token::Slash);
            }
            '(' => {
                chars.next();
                idx += 1;
                tokens.push(Token::LParen);
            }
            ')' => {
                chars.next();
                idx += 1;
                tokens.push(Token::RParen);
            }
            c if c.is_whitespace() => {
                chars.next();
                idx += 1;
            }
            _ => {
                return Err(ParseError {
                    message: format!("unexpected character '{}'", ch),
                    position: idx,
                })
            }
        }
    }

    tokens.push(Token::EOF);
    Ok(tokens)
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::EOF)
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError {
                message: format!("expected {:?}, found {:?}", expected, self.peek()),
                position: self.pos,
            })
        }
    }

    fn parse_expression(&mut self, min_precedence: u8) -> Result<Expr, ParseError> {
        let mut left = self.parse_primary()?;

        while let Some((op, prec)) = self.current_binop() {
            if prec < min_precedence {
                break;
            }
            let next_min_prec = prec + 1;
            self.advance();
            let right = self.parse_expression(next_min_prec)?;
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Token::Number(n) => {
                let value = *n;
                self.advance();
                Ok(Expr::Number(value))
            }
            Token::Minus => {
                // unary minus
                self.advance();
                let expr = self.parse_expression(3)?;
                Ok(Expr::Binary {
                    op: BinOp::Sub,
                    left: Box::new(Expr::Number(0.0)),
                    right: Box::new(expr),
                })
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expression(0)?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            _ => Err(ParseError {
                message: "expected expression".into(),
                position: self.pos,
            }),
        }
    }

    fn current_binop(&self) -> Option<(BinOp, u8)> {
        match self.peek() {
            Token::Plus => Some((BinOp::Add, 1)),
            Token::Minus => Some((BinOp::Sub, 1)),
            Token::Star => Some((BinOp::Mul, 2)),
            Token::Slash => Some((BinOp::Div, 2)),
            _ => None,
        }
    }
}

fn render_js(expr: &Expr, parent_prec: u8) -> String {
    match expr {
        Expr::Number(n) => {
            if n.fract() == 0.0 {
                format!("{}", *n as i64)
            } else {
                format!("{}", n)
            }
        }
        Expr::Binary { op, left, right } => {
            let prec = match op {
                BinOp::Add | BinOp::Sub => 1,
                BinOp::Mul | BinOp::Div => 2,
            };
            let op_str = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
            };
            let l = render_js(left, prec);
            let r = render_js(right, prec);
            let expr_str = format!("{l} {op_str} {r}");
            if prec < parent_prec {
                format!("({expr_str})")
            } else {
                expr_str
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn js(input: &str) -> String {
        transpile(input).unwrap()
    }

    #[test]
    fn parses_numbers() {
        assert_eq!(js("42"), "42");
        assert_eq!(js("3.14"), "3.14");
    }

    #[test]
    fn parses_binary_ops() {
        assert_eq!(js("1+2"), "1 + 2");
        assert_eq!(js("1+2*3"), "1 + 2 * 3");
        assert_eq!(js("(1+2)*3"), "(1 + 2) * 3");
    }

    #[test]
    fn handles_unary_minus() {
        assert_eq!(js("-1+2"), "0 - 1 + 2");
    }

    #[test]
    fn respects_precedence_chain() {
        assert_eq!(js("1-2-3"), "1 - 2 - 3");
        assert_eq!(js("1-2*3-4"), "1 - 2 * 3 - 4");
    }
}
