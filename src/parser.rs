use std::ops::Range;

use crate::ast::{Expr, ExprKind, MatchArm, Pattern, Stmt, Type};
use crate::error::ParseError;
use crate::lexer::{Token, Tok};

pub(crate) struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

pub(crate) fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, ParseError> {
    let mut parser = Parser { tokens, pos: 0 };
    let stmts = parser.parse_program()?;
    match parser.expect(Tok::Eof) {
        Ok(_) => Ok(stmts),
        Err(e) => Err(e),
    }
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

    fn expect(&mut self, expected: Tok) -> Result<Range<usize>, ParseError> {
        if std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(&expected) {
            let span = self.peek().span.clone();
            self.advance();
            Ok(span)
        } else {
            Err(ParseError {
                message: format!("expected {:?}, found {:?}", expected, self.peek().kind),
                span: self.peek().span.clone(),
                source: String::new(),
            })
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while !matches!(self.peek().kind, Tok::Eof) {
            match self.peek().kind {
                Tok::Import => {
                    stmts.push(self.parse_import()?);
                    self.optional_semi();
                }
                Tok::Let => {
                    stmts.push(self.parse_let()?);
                    self.optional_semi();
                }
                Tok::Fn => {
                    stmts.push(self.parse_fn()?);
                    self.optional_semi();
                }
                Tok::External => {
                    stmts.push(self.parse_external()?);
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

    fn parse_import(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Tok::Import)?;
        let path = match &self.peek().kind {
            Tok::Str(s) => {
                let inner = s.trim_matches('"').to_string();
                self.advance();
                inner
            }
            _ => {
                return Err(ParseError {
                    message: "expected import path as string".into(),
                    span: self.peek().span.clone(),
                    source: String::new(),
                })
            }
        };
        Ok(Stmt::Import { path })
    }

    fn optional_semi(&mut self) {
        if matches!(self.peek().kind, Tok::Semi) {
            self.advance();
        }
    }

    fn parse_let(&mut self) -> Result<Stmt, ParseError> {
        let _let_span = self.expect(Tok::Let)?;
        let recursive = if matches!(self.peek().kind, Tok::Rec) {
            self.advance();
            true
        } else {
            false
        };
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
        let ty = if matches!(self.peek().kind, Tok::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(Tok::Eq)?;
        let expr = self.parse_expression(0)?;
        Ok(Stmt::Let { name, ty, expr, recursive })
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
                let pty = if matches!(self.peek().kind, Tok::Colon) {
                    self.advance();
                    Some(self.parse_type()?)
                } else {
                    None
                };
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
        let ret = if matches!(self.peek().kind, Tok::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(Tok::Eq)?;
        let body = self.parse_expression(0)?;
        Ok(Stmt::Fn {
            name,
            params,
            ret,
            body,
        })
    }

    fn parse_external(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Tok::External)?;
        let name = match &self.peek().kind {
            Tok::Ident(s) => {
                let n = s.clone();
                self.advance();
                n
            }
            _ => {
                return Err(ParseError {
                    message: "expected external function name".into(),
                    span: self.peek().span.clone(),
                    source: String::new(),
                })
            }
        };
        self.expect(Tok::Colon)?;
        let ty = self.parse_type()?;
        self.expect(Tok::Eq)?;
        let js_name = match &self.peek().kind {
            Tok::Str(s) => {
                let inner = s.trim_matches('"').to_string();
                self.advance();
                inner
            }
            _ => {
                return Err(ParseError {
                    message: "expected JavaScript function name as string".into(),
                    span: self.peek().span.clone(),
                    source: String::new(),
                })
            }
        };
        Ok(Stmt::External { name, ty, js_name })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let left = self.parse_type_atom()?;
        if matches!(self.peek().kind, Tok::ThinArrow) {
            self.advance();
            let right = self.parse_type()?;
            Ok(Type::Fun(vec![left], Box::new(right)))
        } else {
            Ok(left)
        }
    }

    fn parse_type_atom(&mut self) -> Result<Type, ParseError> {
        match &self.peek().kind {
            Tok::Ident(name) => {
                let ty = match name.as_str() {
                    "Unit" => Type::Unit,
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
            Tok::LParen => {
                let _ = self.expect(Tok::LParen)?;
                if matches!(self.peek().kind, Tok::RParen) {
                    self.advance();
                    return Ok(Type::Unit);
                }
                let first = self.parse_type()?;
                if matches!(self.peek().kind, Tok::Comma) {
                    let mut items = vec![first];
                    while matches!(self.peek().kind, Tok::Comma) {
                        self.advance();
                        let t = self.parse_type()?;
                        items.push(t);
                        if !matches!(self.peek().kind, Tok::Comma) {
                            break;
                        }
                    }
                    self.expect(Tok::RParen)?;
                    if matches!(self.peek().kind, Tok::ThinArrow) {
                        self.advance();
                        let ret = self.parse_type()?;
                        Ok(Type::Fun(items, Box::new(ret)))
                    } else {
                        Ok(Type::Tuple(items))
                    }
                } else {
                    self.expect(Tok::RParen)?;
                    Ok(first)
                }
            }
            Tok::LBracket => {
                self.advance();
                let inner = self.parse_type()?;
                self.expect(Tok::RBracket)?;
                Ok(Type::List(Box::new(inner)))
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
            let span = left.span.start..right.span.end;
            left = Expr {
                span: span.clone(),
                kind: ExprKind::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match &self.peek().kind {
            Tok::Number(n) => {
                let value = *n;
                let span = self.peek().span.clone();
                self.advance();
                Ok(Expr {
                    span,
                    kind: ExprKind::Number(value),
                })
            }
            Tok::Backslash => self.parse_lambda_expr(),
            Tok::True => {
                let span = self.peek().span.clone();
                self.advance();
                Ok(Expr {
                    span,
                    kind: ExprKind::Bool(true),
                })
            }
            Tok::False => {
                let span = self.peek().span.clone();
                self.advance();
                Ok(Expr {
                    span,
                    kind: ExprKind::Bool(false),
                })
            }
            Tok::Str(raw) => {
                let inner = raw.trim_matches('"').to_string();
                let span = self.peek().span.clone();
                self.advance();
                Ok(Expr {
                    span,
                    kind: ExprKind::Str(inner),
                })
            }
            Tok::LBracket => self.parse_list_expr(),
            Tok::LBrace => self.parse_block_expr(),
            Tok::Match => self.parse_match_expr(),
            Tok::Ident(name) => {
                let n = name.clone();
                let span = self.peek().span.clone();
                self.advance();
                if matches!(self.peek().kind, Tok::LParen) {
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
                    let rparen_span = self.expect(Tok::RParen)?;
                    Ok(Expr {
                        span: span.start..rparen_span.end,
                        kind: ExprKind::Call {
                            callee: n,
                            callee_span: span,
                            args,
                        },
                    })
                } else {
                    Ok(Expr {
                        span: span.clone(),
                        kind: ExprKind::Var { name: n },
                    })
                }
            }
            Tok::Rec => {
                let span = self.peek().span.clone();
                self.advance();
                let n = "rec".to_string();
                if matches!(self.peek().kind, Tok::LParen) {
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
                    let rparen_span = self.expect(Tok::RParen)?;
                    Ok(Expr {
                        span: span.start..rparen_span.end,
                        kind: ExprKind::Call {
                            callee: n,
                            callee_span: span,
                            args,
                        },
                    })
                } else {
                    Ok(Expr {
                        span: span.clone(),
                        kind: ExprKind::Var { name: n },
                    })
                }
            }
            Tok::Minus => {
                // unary minus
                let start = self.peek().span.start;
                self.advance();
                let expr = self.parse_expression(7)?;
                Ok(Expr {
                    span: start..expr.span.end,
                    kind: ExprKind::Binary {
                        op: crate::ast::BinOp::Sub,
                        left: Box::new(Expr {
                            span: start..start,
                            kind: ExprKind::Number(0.0),
                        }),
                        right: Box::new(expr),
                    },
                })
            }
            Tok::LParen => self.parse_paren_or_tuple(),
            _ => Err(ParseError {
                message: "expected expression".into(),
                span: self.peek().span.clone(),
                source: String::new(),
            }),
        }
    }

    fn parse_list_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(Tok::LBracket)?.start;
        let mut items = Vec::new();
        if !matches!(self.peek().kind, Tok::RBracket) {
            loop {
                let expr = self.parse_expression(0)?;
                items.push(expr);
                if matches!(self.peek().kind, Tok::Comma) {
                    self.advance();
                    continue;
                } else {
                    break;
                }
            }
        }
        let end = self.expect(Tok::RBracket)?.end;
        Ok(Expr {
            span: start..end,
            kind: ExprKind::List(items),
        })
    }

    fn parse_paren_or_tuple(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(Tok::LParen)?.start;
        let first = self.parse_expression(0)?;
        if matches!(self.peek().kind, Tok::Comma) {
            let mut items = vec![first];
            while matches!(self.peek().kind, Tok::Comma) {
                self.advance();
                let expr = self.parse_expression(0)?;
                items.push(expr);
                if !matches!(self.peek().kind, Tok::Comma) {
                    break;
                }
            }
            let end = self.expect(Tok::RParen)?.end;
            Ok(Expr {
                span: start..end,
                kind: ExprKind::Tuple(items),
            })
        } else {
            let end_span = self.expect(Tok::RParen)?;
            Ok(Expr {
                span: start..end_span.end,
                kind: first.kind,
            })
        }
    }

    fn parse_lambda_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(Tok::Backslash)?.start;
        let mut params = Vec::new();
        if matches!(self.peek().kind, Tok::LParen) {
            self.advance();
            if !matches!(self.peek().kind, Tok::RParen) {
                loop {
                    let pname = self.parse_name()?;
                    let pty = if matches!(self.peek().kind, Tok::Colon) {
                        self.advance();
                        Some(self.parse_type()?)
                    } else {
                        None
                    };
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
        } else {
            let pname = self.parse_name()?;
            let pty = if matches!(self.peek().kind, Tok::Colon) {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };
            params.push((pname, pty));
        }
        self.expect(Tok::ThinArrow)?;
        let body = self.parse_expression(0)?;
        Ok(Expr {
            span: start..body.span.end,
            kind: ExprKind::Lambda {
                params,
                body: Box::new(body),
            },
        })
    }

    fn current_binop(&self) -> Option<(crate::ast::BinOp, u8)> {
        let op = match self.peek().kind {
            Tok::OrOr => Some(crate::ast::BinOp::Or),
            Tok::AndAnd => Some(crate::ast::BinOp::And),
            Tok::EqEq => Some(crate::ast::BinOp::Eq),
            Tok::BangEq => Some(crate::ast::BinOp::Ne),
            Tok::Less => Some(crate::ast::BinOp::Lt),
            Tok::Greater => Some(crate::ast::BinOp::Gt),
            Tok::LessEq => Some(crate::ast::BinOp::Le),
            Tok::GreaterEq => Some(crate::ast::BinOp::Ge),
            Tok::Plus => Some(crate::ast::BinOp::Add),
            Tok::Minus => Some(crate::ast::BinOp::Sub),
            Tok::Star => Some(crate::ast::BinOp::Mul),
            Tok::Slash => Some(crate::ast::BinOp::Div),
            _ => None,
        }?;
        Some((op.clone(), crate::render::binop_precedence(&op)))
    }

    fn parse_block_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(Tok::LBrace)?.start;
        let mut stmts = Vec::new();
        while !matches!(self.peek().kind, Tok::RBrace) {
            if matches!(self.peek().kind, Tok::Let) {
                stmts.push(self.parse_let()?);
                self.optional_semi();
            } else if matches!(self.peek().kind, Tok::Fn) {
                stmts.push(self.parse_fn()?);
                self.optional_semi();
            } else {
                let expr = self.parse_expression(0)?;
                stmts.push(Stmt::Expr(expr));
                self.optional_semi();
            }
        }
        let end = self.expect(Tok::RBrace)?.end;
        if !matches!(stmts.last(), Some(Stmt::Expr(_))) {
            return Err(ParseError {
                message: "block must end with expression".into(),
                span: self.peek().span.clone(),
                source: String::new(),
            });
        }
        Ok(Expr {
            span: start..end,
            kind: ExprKind::Block(stmts),
        })
    }

    fn parse_match_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(Tok::Match)?.start;
        self.expect(Tok::LParen)?;
        let expr = self.parse_expression(0)?;
        let _ = self.expect(Tok::RParen)?;
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
        let end = self.expect(Tok::RBrace)?.end;
        if arms.is_empty() {
            return Err(ParseError {
                message: "match requires at least one arm".into(),
                span: self.peek().span.clone(),
                source: String::new(),
            });
        }
        Ok(Expr {
            span: start..end,
            kind: ExprKind::Match {
                expr: Box::new(expr),
                arms,
            },
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let pat = match &self.peek().kind {
            Tok::Underscore => {
                self.advance();
                Pattern::Wildcard
            }
            Tok::True => {
                self.advance();
                Pattern::Bool(true)
            }
            Tok::False => {
                self.advance();
                Pattern::Bool(false)
            }
            Tok::Number(n) => {
                let value = *n;
                self.advance();
                Pattern::Number(value)
            }
            Tok::Str(raw) => {
                let inner = raw.trim_matches('"').to_string();
                self.advance();
                Pattern::Str(inner)
            }
            Tok::Ident(name) => {
                let n = name.clone();
                self.advance();
                Pattern::Bind(n)
            }
            Tok::Rec => {
                let span = self.peek().span.clone();
                return Err(ParseError {
                    message: "expected pattern".into(),
                    span,
                    source: String::new(),
                });
            }
            Tok::LParen => {
                let _start = self.expect(Tok::LParen)?;
                let first = self.parse_pattern()?;
                if matches!(self.peek().kind, Tok::Comma) {
                    let mut items = vec![first];
                    while matches!(self.peek().kind, Tok::Comma) {
                        self.advance();
                        let p = self.parse_pattern()?;
                        items.push(p);
                        if !matches!(self.peek().kind, Tok::Comma) {
                            break;
                        }
                    }
                    let _ = self.expect(Tok::RParen)?;
                    Pattern::Tuple(items)
                } else {
                    let _ = self.expect(Tok::RParen)?;
                    first
                }
            }
            Tok::LBracket => {
                self.advance();
                let mut items = Vec::new();
                let mut rest = None;
                if !matches!(self.peek().kind, Tok::RBracket) {
                    loop {
                        if matches!(self.peek().kind, Tok::Ellipsis) {
                            self.advance();
                            if let Tok::Ident(s) = &self.peek().kind {
                                rest = Some(s.clone());
                                self.advance();
                            } else {
                                rest = Some(String::new());
                            }
                            break;
                        }
                        let p = self.parse_pattern()?;
                        items.push(p);
                        if matches!(self.peek().kind, Tok::Comma) {
                            self.advance();
                            continue;
                        } else {
                            break;
                        }
                    }
                }
                let _ = self.expect(Tok::RBracket)?;
                Pattern::List { items, rest }
            }
            _ => {
                return Err(ParseError {
                    message: "expected pattern".into(),
                    span: self.peek().span.clone(),
                    source: String::new(),
                })
            }
        };

        Ok(pat)
    }

    fn parse_name(&mut self) -> Result<String, ParseError> {
        match &self.peek().kind {
            Tok::Ident(s) => {
                let n = s.clone();
                self.advance();
                Ok(n)
            }
            Tok::Rec => {
                self.advance();
                Ok("rec".to_string())
            }
            _ => Err(ParseError {
                message: "expected parameter name".into(),
                span: self.peek().span.clone(),
                source: String::new(),
            }),
        }
    }
}
