use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Number,
    String,
    Bool,
    Var(TypeVarId),
    Fun(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    List(Box<Type>),
    Record(Vec<(std::string::String, Type)>),
    Adt { name: String, args: Vec<Type> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVarId(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
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
    Var {
        name: String,
    },
    Str(String),
    Tuple(Vec<Expr>),
    List(Vec<Expr>),
    Block(Vec<Stmt>),
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Call {
        callee: Box<Expr>,
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
    Constructor {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    Record(Vec<(String, Expr)>),
    FieldAccess {
        expr: Box<Expr>,
        field: String,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Bool(bool),
    Number(f64),
    Str(String),
    Tuple(Vec<Pattern>),
    List {
        items: Vec<Pattern>,
        rest: Option<String>,
    },
    Bind(String),
    Constructor {
        name: String,
        fields: Vec<(String, Pattern)>,
    },
    Record {
        fields: Vec<(String, Pattern)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pat: Pattern,
    pub guard: Option<Expr>,
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
pub struct Variant {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Import {
        path: String,
    },
    Let {
        name: String,
        ty: Option<Type>,
        expr: Expr,
        recursive: bool,
    },
    Fn {
        name: String,
        params: Vec<(String, Option<Type>)>,
        ret: Option<Type>,
        body: Expr,
    },
    External {
        name: String,
        ty: Type,
        js_name: String,
    },
    TypeDecl {
        name: String,
        params: Vec<String>,
        variants: Vec<Variant>,
    },
    Expr(Expr),
}
