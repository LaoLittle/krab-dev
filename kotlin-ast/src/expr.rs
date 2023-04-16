use crate::block::Block;
use crate::stmt::Stmt;
use crate::Ident;
use kotlin_span::Span;

#[derive(Debug)]
pub enum ExprStmt {
    Ident(Ident),
    Call(CallExpr),
    Literal,
    Paren(Box<ExprStmt>),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Selector(SelectorExpr),
    Index(IndexExpr),
    Return(ReturnExpr),
    Block(BlockExpr),
    Null,
    Bad,
}

impl ExprStmt {
    pub fn paren(expr: Self) -> Self {
        Self::Paren(expr.into())
    }

    pub fn unary(op: UnaryOp, expr: Self) -> Self {
        Self::Unary(UnaryExpr {
            op,
            expr: expr.into(),
        })
    }

    pub fn binary(op: BinaryOp, lhs: Self, rhs: Self) -> Self {
        Self::Binary(BinaryExpr {
            op,
            lhs: lhs.into(),
            rhs: rhs.into(),
        })
    }

    pub fn call(expr: Self, args: Vec<Self>) -> Self {
        Self::Call(CallExpr {
            expr: expr.into(),
            args,
        })
    }

    pub fn selector(expr: Self, select: Ident) -> Self {
        Self::Selector(SelectorExpr {
            expr: expr.into(),
            select,
        })
    }

    pub fn index(expr: Self, index: Self) -> Self {
        Self::Index(IndexExpr {
            expr: expr.into(),
            index: index.into(),
        })
    }

    pub fn r#return(expr: Option<Self>, at: Option<Ident>) -> Self {
        Self::Return(ReturnExpr {
            expr: expr.map(Box::new),
            at,
        })
    }

    pub fn block(stmts: Vec<Stmt>, at: Option<Ident>, span: Span) -> Self {
        Self::Block(BlockExpr {
            body: Block { stmts, span },
            at,
        })
    }

    #[inline]
    pub const fn null() -> Self {
        Self::Null
    }

    #[inline]
    pub const fn bad() -> Self {
        Self::Bad
    }

    pub fn peel_paren(mut self) -> Self {
        while let ExprStmt::Paren(expr) = self {
            self = *expr;
        }

        self
    }
}

pub enum LiteralExpr {
    Num,
}

#[derive(Debug)]
pub struct SelectorExpr {
    expr: Box<ExprStmt>,
    select: Ident,
}

/// (func)()
///
/// ({..})() lambda can be expr
///
/// (expr)()
#[derive(Debug)]
pub struct CallExpr {
    expr: Box<ExprStmt>, // func can be lambda
    args: Vec<ExprStmt>,
}

#[derive(Debug)]
pub struct UnaryExpr {
    op: UnaryOp,
    expr: Box<ExprStmt>,
}

#[derive(Debug)]
pub enum UnaryOp {
    /// "+"
    Positive,
    /// "-"
    Negative,
    /// "++i"
    PreInc,
    /// "--i"
    PreDec,
    /// "i++"
    PostInc,
    /// "i--"
    PostDec,
    /// "!"
    Not,
}

#[derive(Debug)]
pub struct BinaryExpr {
    op: BinaryOp,
    lhs: Box<ExprStmt>,
    rhs: Box<ExprStmt>,
}

#[derive(Debug)]
pub struct IndexExpr {
    expr: Box<ExprStmt>,
    index: Box<ExprStmt>,
}

#[derive(Debug)]
pub struct ReturnExpr {
    expr: Option<Box<ExprStmt>>,
    at: Option<Ident>,
}

#[derive(Debug)]
pub struct BlockExpr {
    body: Block,
    at: Option<Ident>,
}

#[derive(Debug)]
pub enum BinaryOp {
    /// "+"
    Add,
    /// "-"
    Sub,
    /// "*"
    Mul,
    /// "/"
    Div,
    /// "%"
    Rem,
    /// "&&" (logic and)
    And,
    /// "||" (logic or)
    Or,
    /// "==" (equals to)
    Eq,
    /// "===" (exactly eq)
    ExEq,
    /// ">" (greater then)
    Gt,
    /// ">=" (greater eq)
    Ge,
    /// "<" (less then)
    Lt,
    /// "<=" (less eq)
    Le,
    /// "!=" (not eq)
    Ne,
    /// "!==" (not exactly eq)
    ExNe,
    /// "?:"
    Elvis,
    /// ".."
    Range,
    /// infix function
    Infix,
}

impl BinaryOp {
    pub const fn precedence(&self) -> u32 {
        match self {
            Self::Or => 10,  // Disjunction
            Self::And => 20, // Conjunction
            Self::Eq
            | Self::ExEq
            | Self::Ne
            | Self::ExNe
            | Self::Gt
            | Self::Ge
            | Self::Lt
            | Self::Le => 30, // Comparison
            // 40, Named Checks
            Self::Elvis => 50,                       // Elvis
            Self::Infix => 60,                       // Infix function
            Self::Range => 70,                       // Range
            Self::Add | Self::Sub => 80,             // Additive
            Self::Mul | Self::Div | Self::Rem => 90, // Multiplicative
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::ExprStmt;

    #[test]
    fn peel() {
        let expr = ExprStmt::paren(ExprStmt::paren(ExprStmt::paren(ExprStmt::Null)));
        assert!(matches!(expr.peel_paren(), ExprStmt::Null));
    }
}
