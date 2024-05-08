use kotlin_span::symbol::Symbol;
use kotlin_span::*;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
#[non_exhaustive]
pub enum Type {
    AbstractInt,
    AbstractFloat,
    Refined(Symbol), // This is `Symbol` because `a.b.Class` apparently is not an ident
    Callable(Box<Type>, Vec<Type>),
}

static ALL_INT: [Symbol; 8] = [INT8, INT16, INT32, INT64, UINT8, UINT16, UINT32, UINT64];
static ALL_FP: [Symbol; 2] = [FLOAT32, FLOAT64];

static ALL_SINT: [Symbol; 4] = [INT8, INT16, INT32, INT64];

impl Type {
    pub fn lowering(a: Self, b: Self) -> Option<Self> {
        if a == b {
            return Some(a);
        }

        match (a, b) {
            (Self::Refined(ty), Self::AbstractInt) | (Self::AbstractInt, Self::Refined(ty)) => {
                if ALL_INT.contains(&ty) {
                    return Some(Self::Refined(ty));
                }
            }
            (Self::Refined(ty), Self::AbstractFloat) | (Self::AbstractFloat, Self::Refined(ty)) => {
                if ALL_FP.contains(&ty) {
                    return Some(Self::Refined(ty));
                }
            }
            _ => {}
        }

        None
    }

    pub fn get_refined(self) -> Symbol {
        match self {
            Self::Refined(sym) => sym,
            _ => panic!("required a refined type, found {self:?}"),
        }
    }

    pub fn is_sint(&self) -> bool {
        if let Self::Refined(sym) = self {
            ALL_SINT.contains(sym)
        } else {
            false
        }
    }

    pub fn is_unit(&self) -> bool {
        self == &Self::Refined(UNIT)
    }
}

#[cfg(test)]
mod tests {
    use kotlin_span::{with_global_session_init, UINT64};

    use crate::ty::Type;

    #[test]
    fn lower() {
        with_global_session_init(|| {
            let a = Type::AbstractInt;
            let b = Type::Refined(UINT64);

            assert_eq!(Type::lowering(a, b), Some(Type::Refined(UINT64)));
        });
    }
}
