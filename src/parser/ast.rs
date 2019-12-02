use ordered_float::OrderedFloat;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Ident(pub String);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FullIdent(pub Vec<Ident>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Code(pub Vec<AST>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Array(pub Vec<AST>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Var(pub Ident, pub Box<AST>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    pub ident: Ident,
    pub params: Vec<Ident>,
    pub code: Code,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Call {
    pub ident: FullIdent,
    pub args: Vec<AST>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct If(pub Vec<IfClause>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IfClause {
    pub condition: Box<AST>,
    pub code: Code,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Index {
    pub receiver: Box<AST>,
    pub index: Box<AST>,
}

#[rustfmt::skip]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BinaryOperator { Eq, Gt, Lt, Ge, Le, Add, Sub, Mul, Div, Pow, NotEq, Mod, }

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinaryOperation(pub Box<AST>, pub BinaryOperator, pub Box<AST>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Object(pub Vec<(Ident, AST)>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Assignment(pub FullIdent, pub Box<AST>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ForEach {
    pub binding: Ident,
    pub iterable: Box<AST>,
    pub code: Code,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct While {
    pub condition: Box<AST>,
    pub code: Code,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AST {
    Ident(Ident),
    FullIdent(FullIdent),
    Number(OrderedFloat<f64>),
    Boolean(bool),
    Null,
    String(String),
    Array(Array),
    Var(Var),
    Function(Function),
    Call(Call),
    Index(Index),
    If(If),
    BinaryOp(BinaryOperation),
    Code(Code),
    Object(Object),
    Assignment(Assignment),
    ForEach(ForEach),
    While(While),
    BreakScope(Option<Box<AST>>),
}

impl From<Ident> for FullIdent {
    fn from(i: Ident) -> Self {
        FullIdent(vec![i])
    }
}