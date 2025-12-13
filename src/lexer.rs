use logos::Logos;
use std::ops::Range;

use crate::error::ParseError;

#[derive(Logos, Debug, Clone, PartialEq)]
pub(crate) enum Tok {
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
    #[token("==")]
    EqEq,
    #[token("!=")]
    BangEq,
    #[token("<=")]
    LessEq,
    #[token(">=")]
    GreaterEq,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("=>")]
    Arrow,
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().to_string())]
    Str(String),
    #[token(",")]
    Comma,
    #[token("=")]
    Eq,
    #[token(";")]
    Semi,
    #[token("_", priority = 2)]
    Underscore,
    #[token("match")]
    Match,
    #[token("case")]
    Case,
    #[token("let")]
    Let,
    #[token("fn")]
    Fn,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token(":")]
    Colon,
    #[regex(r"[A-Za-z][A-Za-z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),
    #[regex(r"[ \t\r\n]+", logos::skip)]
    Whitespace,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token {
    pub kind: Tok,
    pub span: Range<usize>,
}

pub(crate) fn lex(input: &str) -> Result<Vec<Token>, ParseError> {
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
