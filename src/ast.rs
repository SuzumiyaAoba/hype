use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Number,
    String,
    Bool,
    Var(TypeVarId),
    Fun(Vec<Type>, Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVarId(pub u32);

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
pub struct Expr {
    pub kind: ExprKind,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Number(f64),
    Bool(bool),
    Var { name: String },
    Str(String),
    Block(Vec<Stmt>),
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Call {
        callee: String,
        callee_span: Range<usize>,
        args: Vec<Expr>,
    },
    Lambda {
        params: Vec<(String, Option<Type>)>,
        body: Box<Expr>,
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

#[derive(Debug, Clone)]
pub struct FnSig {
    pub params: Vec<Type>,
    pub ret: Type,
}

#[derive(Debug, Clone)]
pub struct Scheme {
    pub vars: Vec<TypeVarId>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let { name: String, ty: Option<Type>, expr: Expr, recursive: bool },
    Fn {
        name: String,
        params: Vec<(String, Option<Type>)>,
        ret: Option<Type>,
        body: Expr,
    },
    Expr(Expr),
}
