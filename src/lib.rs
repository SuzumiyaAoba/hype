use anstyle::{AnsiColor, Reset, Style};
use logos::Logos;
use std::ops::Range;

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

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Range<usize>,
    pub source: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ParseError {}

#[derive(Logos, Debug, Clone, PartialEq)]
enum Tok {
    #[regex(r"[0-9]+(\.[0-9]+)?", |lex| lex.slice().parse::<f64>().unwrap())]
    Number(f64),
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[regex(r"[ \t\r\n]+", logos::skip)]
    Whitespace,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
struct Token {
    kind: Tok,
    span: Range<usize>,
}

pub fn transpile(input: &str) -> Result<String, ParseError> {
    let tokens = lex(input)?;
    let mut parser = Parser { tokens, pos: 0 };
    match parser.parse_expression(0) {
        Ok(expr) => {
            match parser.expect(Tok::Eof) {
                Ok(_) => Ok(render_js(&expr, 0)),
                Err(mut e) => {
                    if e.source.is_empty() {
                        e.source = input.to_string();
                    }
                    Err(e)
                }
            }
        }
        Err(mut e) => {
            if e.source.is_empty() {
                e.source = input.to_string();
            }
            Err(e)
        }
    }
}

fn lex(input: &str) -> Result<Vec<Token>, ParseError> {
    let mut lex = Tok::lexer(input);
    let mut out = Vec::new();
    while let Some(res) = lex.next() {
        let span = lex.span();
        match res {
            Ok(tok) => match tok {
                Tok::Whitespace => {}
                _ => out.push(Token { kind: tok, span }),
            },
            Err(_) => {
                return Err(ParseError {
                    message: "invalid token".into(),
                    span,
                    source: input.to_string(),
                })
            }
        }
    }
    // push EOF sentinel
    out.push(Token {
        kind: Tok::Eof, // sentinel
        span: input.len()..input.len(),
    });
    Ok(out)
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).expect("sentinel present")
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: Tok) -> Result<(), ParseError> {
        if std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError {
                message: format!("expected {:?}, found {:?}", expected, self.peek().kind),
                span: self.peek().span.clone(),
                source: String::new(), // not used by format_error because caller supplies source
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
        match &self.peek().kind {
            Tok::Number(n) => {
                let value = *n;
                self.advance();
                Ok(Expr::Number(value))
            }
            Tok::Minus => {
                // unary minus
                self.advance();
                let expr = self.parse_expression(3)?;
                Ok(Expr::Binary {
                    op: BinOp::Sub,
                    left: Box::new(Expr::Number(0.0)),
                    right: Box::new(expr),
                })
            }
            Tok::LParen => {
                self.advance();
                let expr = self.parse_expression(0)?;
                self.expect(Tok::RParen)?;
                Ok(expr)
            }
            _ => Err(ParseError {
                message: "expected expression".into(),
                span: self.peek().span.clone(),
                source: String::new(),
            }),
        }
    }

    fn current_binop(&self) -> Option<(BinOp, u8)> {
        match self.peek().kind {
            Tok::Plus => Some((BinOp::Add, 1)),
            Tok::Minus => Some((BinOp::Sub, 1)),
            Tok::Star => Some((BinOp::Mul, 2)),
            Tok::Slash => Some((BinOp::Div, 2)),
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

/// エラーをカラフルに整形（赤アンダーカールと ^ を表示）。
pub fn format_error(err: &ParseError) -> String {
    let (line, col, line_start, line_end) = line_info(&err.source, err.span.start);
    let line_str = &err.source[line_start..line_end];
    let caret_pad = " ".repeat(col.saturating_sub(1));

    let red_bold = Style::new().fg_color(Some(AnsiColor::Red.into())).bold();
    let dim = Style::new().fg_color(Some(AnsiColor::BrightBlack.into()));
    let reset = Reset.render();
    const UNDERCURL_RED: &str = "\u{001b}[31;4:3m";

    let under = underline_slice(line_str, col, err.span.len().max(1));

    format!(
        "{hdr} error{reset}\n{dim}│{reset}  at line {line}, col {col}\n{dim}│{reset}  {pre}{curl}{target}{reset}{post}\n{dim}└{reset}  {pad}{caret}\n   {pad}{msg}",
        hdr = red_bold.render(),
        line = line,
        col = col,
        pre = under.pre,
        target = under.target,
        post = under.post,
        curl = UNDERCURL_RED,
        dim = dim.render(),
        reset = reset,
        pad = caret_pad,
        caret = format!("{}^{}", red_bold.render(), reset),
        msg = err.message
    )
}

struct Underline<'a> {
    pre: &'a str,
    target: &'a str,
    post: &'a str,
}

fn underline_slice<'a>(line: &'a str, col: usize, len: usize) -> Underline<'a> {
    let mut start_byte = line.len();
    let mut end_byte = line.len();
    let mut current_col = 1usize;

    for (i, _) in line.char_indices() {
        if current_col == col {
            start_byte = i;
            break;
        }
        current_col += 1;
    }

    if start_byte == line.len() && col == 1 {
        start_byte = 0;
    }

    let mut count = 0usize;
    for (i, _) in line[start_byte..].char_indices() {
        if count == len {
            end_byte = start_byte + i;
            break;
        }
        count += 1;
    }
    if count < len {
        end_byte = line.len();
    }

    let pre = &line[..start_byte.min(line.len())];
    let target = &line[start_byte.min(line.len())..end_byte.min(line.len())];
    let post = &line[end_byte.min(line.len())..];

    Underline { pre, target, post }
}

fn line_info(src: &str, offset: usize) -> (usize, usize, usize, usize) {
    let mut line = 1usize;
    let mut col = 1usize;
    let mut last_line_start = 0usize;
    for (i, ch) in src.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
            last_line_start = i + ch.len_utf8();
        } else {
            col += 1;
        }
    }

    let line_end = src[last_line_start..]
        .find('\n')
        .map(|idx| last_line_start + idx)
        .unwrap_or_else(|| src.len());

    (line, col, last_line_start, line_end)
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
