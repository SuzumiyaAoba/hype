use anstyle::{AnsiColor, Reset, Style};
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

#[derive(Debug, Clone, Copy, PartialEq)]
struct Span {
    offset: usize,
    line: usize,
    col: usize,
}

#[derive(Debug, Clone, PartialEq, Error)]
#[error("{message} at line {line}, col {col}")]
pub struct ParseError {
    pub message: String,
    pub position: usize,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum TokenKind {
    Number(f64),
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
struct Token {
    kind: TokenKind,
    span: Span,
}

pub fn transpile(input: &str) -> Result<String, ParseError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser { tokens, pos: 0 };
    let expr = parser.parse_expression(0)?;
    parser.expect(TokenKind::EOF)?;
    Ok(render_js(&expr, 0))
}

/// エラーを行・列付きでUnicodeシンボルとともに整形する。
pub fn format_error(input: &str, err: &ParseError) -> String {
    let line_str = input
        .lines()
        .nth(err.line.saturating_sub(1))
        .unwrap_or("");
    let arrow_col = err.col.max(1);
    let caret_pad = " ".repeat(arrow_col.saturating_sub(1));

    let red_bold = Style::new().fg_color(Some(AnsiColor::Red.into())).bold();
    let dim = Style::new().fg_color(Some(AnsiColor::BrightBlack.into()));
    let reset = Reset.render();

    format!(
        "{hdr} error:{reset} {msg}\n{dim}│{reset}  at line {line}, col {col}\n{dim}│{reset}  {text}\n{dim}└─{reset} {pad}{carat}",
        hdr = red_bold.render(),
        msg = err.message,
        line = err.line,
        col = err.col,
        text = line_str,
        pad = caret_pad,
        carat = format!("{}^{}", red_bold.render(), reset),
        dim = dim.render(),
        reset = reset
    )
}

fn tokenize(input: &str) -> Result<Vec<Token>, ParseError> {
    let mut chars = input.chars().peekable();
    let mut tokens = Vec::new();
    let mut offset = 0usize;
    let mut line = 1usize;
    let mut col = 1usize;

    while let Some(&ch) = chars.peek() {
        let span = Span { offset, line, col };
        match ch {
            '0'..='9' | '.' => {
                let mut buf = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() || c == '.' {
                        buf.push(c);
                        chars.next();
                        offset += 1;
                        col += 1;
                    } else {
                        break;
                    }
                }
                let value: f64 = buf.parse().map_err(|_| ParseError {
                    message: "invalid number".into(),
                    position: span.offset,
                    line: span.line,
                    col: span.col,
                })?;
                tokens.push(Token {
                    kind: TokenKind::Number(value),
                    span,
                });
            }
            '+' | '-' | '*' | '/' | '(' | ')' => {
                let kind = match ch {
                    '+' => TokenKind::Plus,
                    '-' => TokenKind::Minus,
                    '*' => TokenKind::Star,
                    '/' => TokenKind::Slash,
                    '(' => TokenKind::LParen,
                    ')' => TokenKind::RParen,
                    _ => unreachable!(),
                };
                chars.next();
                offset += 1;
                col += 1;
                tokens.push(Token { kind, span });
            }
            '\n' => {
                chars.next();
                offset += 1;
                line += 1;
                col = 1;
            }
            c if c.is_whitespace() => {
                chars.next();
                offset += 1;
                col += 1;
            }
            _ => {
                return Err(ParseError {
                    message: format!("unexpected character '{}'", ch),
                    position: span.offset,
                    line: span.line,
                    col: span.col,
                })
            }
        }
    }

    tokens.push(Token {
        kind: TokenKind::EOF,
        span: Span { offset, line, col },
    });
    Ok(tokens)
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(self.tokens.last().expect("tokens non-empty"))
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        if std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            let t = self.peek();
            Err(ParseError {
                message: format!("expected {:?}, found {:?}", expected, t.kind),
                position: t.span.offset,
                line: t.span.line,
                col: t.span.col,
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
            Token { kind: TokenKind::Number(n), .. } => {
                let value = *n;
                self.advance();
                Ok(Expr::Number(value))
            }
            Token { kind: TokenKind::Minus, .. } => {
                // unary minus
                self.advance();
                let expr = self.parse_expression(3)?;
                Ok(Expr::Binary {
                    op: BinOp::Sub,
                    left: Box::new(Expr::Number(0.0)),
                    right: Box::new(expr),
                })
            }
            Token { kind: TokenKind::LParen, .. } => {
                self.advance();
                let expr = self.parse_expression(0)?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            _ => Err(ParseError {
                message: "expected expression".into(),
                position: self.peek().span.offset,
                line: self.peek().span.line,
                col: self.peek().span.col,
            }),
        }
    }

    fn current_binop(&self) -> Option<(BinOp, u8)> {
        match self.peek() {
            Token { kind: TokenKind::Plus, .. } => Some((BinOp::Add, 1)),
            Token { kind: TokenKind::Minus, .. } => Some((BinOp::Sub, 1)),
            Token { kind: TokenKind::Star, .. } => Some((BinOp::Mul, 2)),
            Token { kind: TokenKind::Slash, .. } => Some((BinOp::Div, 2)),
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
