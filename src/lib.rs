use anstyle::{AnsiColor, Reset, Style};
use logos::Logos;
use std::collections::HashMap;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Bool(bool),
    Var(String),
    Str(String),
    Block(Vec<Stmt>),
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Call {
        callee: String,
        args: Vec<Expr>,
    },
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Bool(bool),
    Number(f64),
    Str(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pat: Pattern,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let { name: String, ty: Type, expr: Expr },
    Fn {
        name: String,
        params: Vec<(String, Type)>,
        ret: Type,
        body: Expr,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Number,
    String,
    Bool,
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
struct Token {
    kind: Tok,
    span: Range<usize>,
}

pub fn transpile(input: &str) -> Result<String, ParseError> {
    let tokens = lex(input)?;
    let mut parser = Parser { tokens, pos: 0 };
    match parser.parse_program() {
        Ok(stmts) => {
            typecheck(&stmts, input)?;
            match parser.expect(Tok::Eof) {
                Ok(_) => Ok(render_program(&stmts)),
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

    fn parse_program(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while !matches!(self.peek().kind, Tok::Eof) {
            match self.peek().kind {
                Tok::Let => {
                    stmts.push(self.parse_let()?);
                    self.optional_semi();
                }
                Tok::Fn => {
                    stmts.push(self.parse_fn()?);
                    self.optional_semi();
                }
                _ => {
                    let expr = self.parse_expression(0)?;
                    stmts.push(Stmt::Expr(expr));
                    self.optional_semi();
                }
            }
        }
        Ok(stmts)
    }

    fn optional_semi(&mut self) {
        if matches!(self.peek().kind, Tok::Semi) {
            self.advance();
        }
    }

    fn parse_let(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Tok::Let)?;
        let name = match &self.peek().kind {
            Tok::Ident(s) => {
                let n = s.clone();
                self.advance();
                n
            }
            _ => {
                return Err(ParseError {
                    message: "expected identifier".into(),
                    span: self.peek().span.clone(),
                    source: String::new(),
                })
            }
        };
        self.expect(Tok::Colon)?;
        let ty = self.parse_type()?;
        self.expect(Tok::Eq)?;
        let expr = self.parse_expression(0)?;
        Ok(Stmt::Let { name, ty, expr })
    }

    fn parse_fn(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Tok::Fn)?;
        let name = match &self.peek().kind {
            Tok::Ident(s) => {
                let n = s.clone();
                self.advance();
                n
            }
            _ => {
                return Err(ParseError {
                    message: "expected function name".into(),
                    span: self.peek().span.clone(),
                    source: String::new(),
                })
            }
        };
        self.expect(Tok::LParen)?;
        let mut params = Vec::new();
        if !matches!(self.peek().kind, Tok::RParen) {
            loop {
                let pname = match &self.peek().kind {
                    Tok::Ident(s) => {
                        let n = s.clone();
                        self.advance();
                        n
                    }
                    _ => {
                        return Err(ParseError {
                            message: "expected parameter name".into(),
                            span: self.peek().span.clone(),
                            source: String::new(),
                        })
                    }
                };
                self.expect(Tok::Colon)?;
                let pty = self.parse_type()?;
                params.push((pname, pty));
                if matches!(self.peek().kind, Tok::Comma) {
                    self.advance();
                    continue;
                } else {
                    break;
                }
            }
        }
        self.expect(Tok::RParen)?;
        self.expect(Tok::Colon)?;
        let ret = self.parse_type()?;
        self.expect(Tok::Eq)?;
        let body = self.parse_expression(0)?;
        Ok(Stmt::Fn {
            name,
            params,
            ret,
            body,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match &self.peek().kind {
            Tok::Ident(name) => {
                let ty = match name.as_str() {
                    "Number" => Type::Number,
                    "String" => Type::String,
                    "Bool" => Type::Bool,
                    _ => {
                        return Err(ParseError {
                            message: format!("unknown type '{}'", name),
                            span: self.peek().span.clone(),
                            source: String::new(),
                        })
                    }
                };
                self.advance();
                Ok(ty)
            }
            _ => Err(ParseError {
                message: "expected type".into(),
                span: self.peek().span.clone(),
                source: String::new(),
            }),
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
            Tok::True => {
                self.advance();
                Ok(Expr::Bool(true))
            }
            Tok::False => {
                self.advance();
                Ok(Expr::Bool(false))
            }
            Tok::Str(raw) => {
                let inner = raw.trim_matches('"').to_string();
                self.advance();
                Ok(Expr::Str(inner))
            }
            Tok::LBrace => self.parse_block_expr(),
            Tok::Match => self.parse_match_expr(),
            Tok::Ident(name) => {
                let n = name.clone();
                self.advance();
                if matches!(self.peek().kind, Tok::LParen) {
                    // function call
                    self.advance(); // consume '('
                    let mut args = Vec::new();
                    if !matches!(self.peek().kind, Tok::RParen) {
                        loop {
                            let arg = self.parse_expression(0)?;
                            args.push(arg);
                            if matches!(self.peek().kind, Tok::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(Tok::RParen)?;
                    Ok(Expr::Call { callee: n, args })
                } else {
                    Ok(Expr::Var(n))
                }
            }
            Tok::Minus => {
                // unary minus
                self.advance();
                let expr = self.parse_expression(7)?;
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

    fn parse_block_expr(&mut self) -> Result<Expr, ParseError> {
        self.expect(Tok::LBrace)?;
        let mut stmts = Vec::new();
        while !matches!(self.peek().kind, Tok::RBrace) {
            if matches!(self.peek().kind, Tok::Let) {
                stmts.push(self.parse_let()?);
                self.optional_semi();
            } else {
                let expr = self.parse_expression(0)?;
                stmts.push(Stmt::Expr(expr));
                self.optional_semi();
            }
        }
        self.expect(Tok::RBrace)?;
        if !matches!(stmts.last(), Some(Stmt::Expr(_))) {
            return Err(ParseError {
                message: "block must end with expression".into(),
                span: self.peek().span.clone(),
                source: String::new(),
            });
        }
        Ok(Expr::Block(stmts))
    }

    fn parse_match_expr(&mut self) -> Result<Expr, ParseError> {
        self.expect(Tok::Match)?;
        self.expect(Tok::LParen)?;
        let expr = self.parse_expression(0)?;
        self.expect(Tok::RParen)?;
        self.expect(Tok::LBrace)?;
        let mut arms = Vec::new();
        while !matches!(self.peek().kind, Tok::RBrace) {
            self.expect(Tok::Case)?;
            let pat = self.parse_pattern()?;
            self.expect(Tok::Arrow)?;
            let body = self.parse_expression(0)?;
            arms.push(MatchArm { pat, expr: body });
            self.optional_semi();
        }
        self.expect(Tok::RBrace)?;
        if arms.is_empty() {
            return Err(ParseError {
                message: "match requires at least one arm".into(),
                span: self.peek().span.clone(),
                source: String::new(),
            });
        }
        Ok(Expr::Match {
            expr: Box::new(expr),
            arms,
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match &self.peek().kind {
            Tok::Underscore => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            Tok::True => {
                self.advance();
                Ok(Pattern::Bool(true))
            }
            Tok::False => {
                self.advance();
                Ok(Pattern::Bool(false))
            }
            Tok::Number(n) => {
                let value = *n;
                self.advance();
                Ok(Pattern::Number(value))
            }
            Tok::Str(raw) => {
                let inner = raw.trim_matches('"').to_string();
                self.advance();
                Ok(Pattern::Str(inner))
            }
            _ => Err(ParseError {
                message: "expected pattern".into(),
                span: self.peek().span.clone(),
                source: String::new(),
            }),
        }
    }

    fn current_binop(&self) -> Option<(BinOp, u8)> {
        let op = match self.peek().kind {
            Tok::OrOr => Some(BinOp::Or),
            Tok::AndAnd => Some(BinOp::And),
            Tok::EqEq => Some(BinOp::Eq),
            Tok::BangEq => Some(BinOp::Ne),
            Tok::Less => Some(BinOp::Lt),
            Tok::Greater => Some(BinOp::Gt),
            Tok::LessEq => Some(BinOp::Le),
            Tok::GreaterEq => Some(BinOp::Ge),
            Tok::Plus => Some(BinOp::Add),
            Tok::Minus => Some(BinOp::Sub),
            Tok::Star => Some(BinOp::Mul),
            Tok::Slash => Some(BinOp::Div),
            _ => None,
        }?;
        Some((op.clone(), binop_precedence(&op)))
    }
}

fn binop_precedence(op: &BinOp) -> u8 {
    match op {
        BinOp::Or => 1,
        BinOp::And => 2,
        BinOp::Eq | BinOp::Ne => 3,
        BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => 4,
        BinOp::Add | BinOp::Sub => 5,
        BinOp::Mul | BinOp::Div => 6,
    }
}

fn binop_symbol(op: &BinOp) -> &'static str {
    match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Eq => "==",
        BinOp::Ne => "!=",
        BinOp::Lt => "<",
        BinOp::Gt => ">",
        BinOp::Le => "<=",
        BinOp::Ge => ">=",
        BinOp::And => "&&",
        BinOp::Or => "||",
    }
}

fn render_number_literal(n: f64) -> String {
    if n.fract() == 0.0 {
        format!("{}", n as i64)
    } else {
        format!("{}", n)
    }
}

fn render_pattern_condition(var: &str, pat: &Pattern) -> String {
    match pat {
        Pattern::Wildcard => "true".to_string(),
        Pattern::Bool(b) => format!("{var} === {b}"),
        Pattern::Number(n) => format!("{var} === {}", render_number_literal(*n)),
        Pattern::Str(s) => format!("{var} === \"{}\"", escape_js_str(s)),
    }
}

fn render_block(stmts: &[Stmt]) -> String {
    let mut parts = Vec::new();
    let last_expr = match stmts.last() {
        Some(Stmt::Expr(e)) => e,
        _ => unreachable!("parser enforces block ending with expression"),
    };
    for stmt in stmts.iter().take(stmts.len().saturating_sub(1)) {
        match stmt {
            Stmt::Let { name, expr, .. } => {
                let js = render_js(expr, 0);
                parts.push(format!("let {name} = {js};"));
            }
            Stmt::Expr(expr) => {
                let js = render_js(expr, 0);
                parts.push(format!("{js};"));
            }
            Stmt::Fn { .. } => {
                // 未サポート
            }
        }
    }
    let last = render_js(last_expr, 0);
    parts.push(format!("return {last};"));
    format!("(() => {{ {} }})()", parts.join(" "))
}

fn render_match(expr: &Expr, arms: &[MatchArm]) -> String {
    let scrutinee = render_js(expr, 0);
    let mut parts = Vec::new();
    parts.push(format!("const __match = {scrutinee};"));
    for (i, arm) in arms.iter().enumerate() {
        let cond = render_pattern_condition("__match", &arm.pat);
        let body = render_js(&arm.expr, 0);
        if i == 0 {
            parts.push(format!("if ({cond}) {{ return {body}; }}"));
        } else {
            parts.push(format!("else if ({cond}) {{ return {body}; }}"));
        }
    }
    parts.push("return undefined;".to_string());
    format!("(() => {{ {} }})()", parts.join(" "))
}

fn render_js(expr: &Expr, parent_prec: u8) -> String {
    match expr {
        Expr::Number(n) => {
            render_number_literal(*n)
        }
        Expr::Bool(b) => format!("{b}"),
        Expr::Str(s) => format!("\"{}\"", escape_js_str(s)),
        Expr::Var(name) => name.clone(),
        Expr::Call { callee, args } => {
            let rendered_args: Vec<String> = args.iter().map(|a| render_js(a, 0)).collect();
            format!("{callee}({})", rendered_args.join(", "))
        }
        Expr::Block(stmts) => render_block(stmts),
        Expr::Match { expr, arms } => render_match(expr, arms),
        Expr::Binary { op, left, right } => {
            let prec = binop_precedence(op);
            let op_str = binop_symbol(op);
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

fn render_program(stmts: &[Stmt]) -> String {
    let mut lines = Vec::new();
    for (i, stmt) in stmts.iter().enumerate() {
        match stmt {
            Stmt::Let { name, expr, .. } => {
                let js = render_js(expr, 0);
                lines.push(format!("let {name} = {js};"));
            }
            Stmt::Fn { name, params, body, .. } => {
                let js_body = render_js(body, 0);
                let param_list: Vec<String> = params.iter().map(|(p, _)| p.clone()).collect();
                lines.push(format!(
                    "function {name}({}) {{ return {js_body}; }}",
                    param_list.join(", ")
                ));
            }
            Stmt::Expr(expr) => {
                let js = render_js(expr, 0);
                // 最終式だけセミコロンを付けない
                if i == stmts.len() - 1 {
                    lines.push(js);
                } else {
                    lines.push(format!("{js};"));
                }
            }
        }
    }
    lines.join("\n")
}

fn escape_js_str(s: &str) -> String {
    s.chars()
        .flat_map(|c| match c {
            '\\' => "\\\\".chars().collect::<Vec<_>>(),
            '"' => "\\\"".chars().collect::<Vec<_>>(),
            '\n' => "\\n".chars().collect::<Vec<_>>(),
            '\r' => "\\r".chars().collect::<Vec<_>>(),
            '\t' => "\\t".chars().collect::<Vec<_>>(),
            other => vec![other],
        })
        .collect()
}

#[derive(Debug, Clone)]
struct FnSig {
    params: Vec<Type>,
    ret: Type,
}

fn typecheck(stmts: &[Stmt], source: &str) -> Result<(), ParseError> {
    let mut fns: HashMap<String, FnSig> = HashMap::new();
    for stmt in stmts {
        if let Stmt::Fn { name, params, ret, .. } = stmt {
            if fns.contains_key(name) {
                return Err(ParseError {
                    message: format!("duplicate function '{}'", name),
                    span: 0..0,
                    source: source.to_string(),
                });
            }
            let param_tys = params.iter().map(|(_, t)| t.clone()).collect();
            fns.insert(
                name.clone(),
                FnSig {
                    params: param_tys,
                    ret: ret.clone(),
                },
            );
        }
    }

    let mut vars: HashMap<String, Type> = HashMap::new();
    for stmt in stmts {
        match stmt {
            Stmt::Let { name, ty, expr } => {
                let expr_ty = type_of_expr(expr, &vars, &fns, source)?;
                if &expr_ty != ty {
                    return Err(ParseError {
                        message: format!("type mismatch: expected {:?}, found {:?}", ty, expr_ty),
                        span: 0..0,
                        source: source.to_string(),
                    });
                }
                vars.insert(name.clone(), ty.clone());
            }
            Stmt::Fn { name: _, params, ret, body } => {
                let mut local = vars.clone();
                for (pname, pty) in params {
                    local.insert(pname.clone(), pty.clone());
                }
                let body_ty = type_of_expr(body, &local, &fns, source)?;
                if &body_ty != ret {
                    return Err(ParseError {
                        message: format!(
                            "type mismatch in function body: expected {:?}, found {:?}",
                            ret, body_ty
                        ),
                        span: 0..0,
                        source: source.to_string(),
                    });
                }
            }
            Stmt::Expr(expr) => {
                let _ = type_of_expr(expr, &vars, &fns, source)?;
            }
        }
    }
    Ok(())
}

fn type_of_expr(
    expr: &Expr,
    vars: &HashMap<String, Type>,
    fns: &HashMap<String, FnSig>,
    source: &str,
) -> Result<Type, ParseError> {
    match expr {
        Expr::Number(_) => Ok(Type::Number),
        Expr::Bool(_) => Ok(Type::Bool),
        Expr::Str(_) => Ok(Type::String),
        Expr::Block(stmts) => type_of_block(stmts, vars, fns, source),
        Expr::Match { expr, arms } => type_of_match(expr, arms, vars, fns, source),
        Expr::Var(name) => vars.get(name).cloned().ok_or_else(|| ParseError {
            message: format!("unbound variable '{}'", name),
            span: 0..0,
            source: source.to_string(),
        }),
        Expr::Binary { op, left, right } => {
            let lt = type_of_expr(left, vars, fns, source)?;
            let rt = type_of_expr(right, vars, fns, source)?;
            match op {
                BinOp::Add => {
                    if lt == Type::Number && rt == Type::Number {
                        Ok(Type::Number)
                    } else if lt == Type::String && rt == Type::String {
                        Ok(Type::String)
                    } else {
                        Err(ParseError {
                            message: "type mismatch for '+'".into(),
                            span: 0..0,
                            source: source.to_string(),
                        })
                    }
                }
                BinOp::Sub | BinOp::Mul | BinOp::Div => {
                    if lt == Type::Number && rt == Type::Number {
                        Ok(Type::Number)
                    } else {
                        Err(ParseError {
                            message: "numeric operator requires Number".into(),
                            span: 0..0,
                            source: source.to_string(),
                        })
                    }
                }
                BinOp::Eq | BinOp::Ne => {
                    if lt == rt {
                        Ok(Type::Bool)
                    } else {
                        Err(ParseError {
                            message: "equality requires operands of the same type".into(),
                            span: 0..0,
                            source: source.to_string(),
                        })
                    }
                }
                BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
                    if lt == Type::Number && rt == Type::Number {
                        Ok(Type::Bool)
                    } else {
                        Err(ParseError {
                            message: "comparison requires Number operands".into(),
                            span: 0..0,
                            source: source.to_string(),
                        })
                    }
                }
                BinOp::And | BinOp::Or => {
                    if lt == Type::Bool && rt == Type::Bool {
                        Ok(Type::Bool)
                    } else {
                        Err(ParseError {
                            message: "logical operator requires Bool".into(),
                            span: 0..0,
                            source: source.to_string(),
                        })
                    }
                }
            }
        }
        Expr::Call { callee, args } => {
            let sig = fns.get(callee).ok_or_else(|| ParseError {
                message: format!("unknown function '{}'", callee),
                span: 0..0,
                source: source.to_string(),
            })?;
            if sig.params.len() != args.len() {
                return Err(ParseError {
                    message: format!(
                        "arity mismatch: expected {}, found {}",
                        sig.params.len(),
                        args.len()
                    ),
                    span: 0..0,
                    source: source.to_string(),
                });
            }
            for (arg, expected) in args.iter().zip(sig.params.iter()) {
                let aty = type_of_expr(arg, vars, fns, source)?;
                if &aty != expected {
                    return Err(ParseError {
                        message: format!(
                            "type mismatch in argument: expected {:?}, found {:?}",
                            expected, aty
                        ),
                        span: 0..0,
                        source: source.to_string(),
                    });
                }
            }
            Ok(sig.ret.clone())
        }
    }
}

fn type_of_block(
    stmts: &[Stmt],
    vars: &HashMap<String, Type>,
    fns: &HashMap<String, FnSig>,
    source: &str,
) -> Result<Type, ParseError> {
    if !matches!(stmts.last(), Some(Stmt::Expr(_))) {
        return Err(ParseError {
            message: "block must end with expression".into(),
            span: 0..0,
            source: source.to_string(),
        });
    }
    let mut local = vars.clone();
    let mut last_ty = None;
    for stmt in stmts {
        match stmt {
            Stmt::Let { name, ty, expr } => {
                let expr_ty = type_of_expr(expr, &local, fns, source)?;
                if &expr_ty != ty {
                    return Err(ParseError {
                        message: format!("type mismatch: expected {:?}, found {:?}", ty, expr_ty),
                        span: 0..0,
                        source: source.to_string(),
                    });
                }
                local.insert(name.clone(), ty.clone());
            }
            Stmt::Expr(expr) => {
                let ty = type_of_expr(expr, &local, fns, source)?;
                last_ty = Some(ty);
            }
            Stmt::Fn { .. } => {
                return Err(ParseError {
                    message: "functions inside blocks are not supported".into(),
                    span: 0..0,
                    source: source.to_string(),
                });
            }
        }
    }
    last_ty.ok_or_else(|| ParseError {
        message: "block must end with expression".into(),
        span: 0..0,
        source: source.to_string(),
    })
}

fn ensure_pattern_matches(
    pat: &Pattern,
    ty: &Type,
    source: &str,
) -> Result<(), ParseError> {
    let ok = match pat {
        Pattern::Wildcard => true,
        Pattern::Bool(_) => matches!(ty, Type::Bool),
        Pattern::Number(_) => matches!(ty, Type::Number),
        Pattern::Str(_) => matches!(ty, Type::String),
    };
    if ok {
        Ok(())
    } else {
        Err(ParseError {
            message: format!("pattern does not match type {:?}", ty),
            span: 0..0,
            source: source.to_string(),
        })
    }
}

fn type_of_match(
    expr: &Expr,
    arms: &[MatchArm],
    vars: &HashMap<String, Type>,
    fns: &HashMap<String, FnSig>,
    source: &str,
) -> Result<Type, ParseError> {
    if arms.is_empty() {
        return Err(ParseError {
            message: "match requires at least one arm".into(),
            span: 0..0,
            source: source.to_string(),
        });
    }

    let scrutinee_ty = type_of_expr(expr, vars, fns, source)?;
    let mut result_ty: Option<Type> = None;
    for arm in arms {
        ensure_pattern_matches(&arm.pat, &scrutinee_ty, source)?;
        let ty = type_of_expr(&arm.expr, vars, fns, source)?;
        if let Some(prev) = &result_ty {
            if prev != &ty {
                return Err(ParseError {
                    message: format!(
                        "match arms must have the same type, found {:?} and {:?}",
                        prev, ty
                    ),
                    span: 0..0,
                    source: source.to_string(),
                });
            }
        } else {
            result_ty = Some(ty);
        }
    }

    result_ty.ok_or_else(|| ParseError {
        message: "match requires at least one arm".into(),
        span: 0..0,
        source: source.to_string(),
    })
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

    #[test]
    fn bool_literals_and_logic() {
        assert_eq!(js("true && false || true"), "true && false || true");
    }

    #[test]
    fn comparisons_and_equality() {
        assert_eq!(js("1 < 2 == true"), "1 < 2 == true");
    }

    #[test]
    fn lets_and_vars() {
        let out = js("let x: Number = 1+2; let y: Number = x*3; y - x");
        assert_eq!(out, "let x = 1 + 2;\nlet y = x * 3;\ny - x");
    }

    #[test]
    fn function_call_with_bool_return() {
        let out = js("fn eq(a: Number, b: Number): Bool = a == b; eq(1, 1)");
        assert_eq!(out, "function eq(a, b) { return a == b; }\neq(1, 1)");
    }

    #[test]
    fn block_expression_returns_last_expr() {
        let out = js("{ let x: Number = 1; x + 2 }");
        assert_eq!(out, "(() => { let x = 1; return x + 2; })()");
    }

    #[test]
    fn match_expression_over_bool() {
        let out = js("match(true){case true => { let x: Number = 1; x }; case _ => 0 }");
        assert_eq!(
            out,
            "(() => { const __match = true; if (__match === true) { return (() => { let x = 1; return x; })(); } else if (true) { return 0; } return undefined; })()"
        );
    }
}
