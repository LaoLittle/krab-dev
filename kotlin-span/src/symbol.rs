use crate::with_global_session;
use std::borrow::Cow;
use std::fmt::{Debug, Formatter};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(pub(crate) usize);

pub type SymbolName = Cow<'static, str>;

impl Symbol {
    #[inline]
    pub const fn new(i: usize) -> Self {
        Self(i)
    }

    #[inline]
    pub const fn as_index(self) -> usize {
        self.0
    }

    pub fn intern(s: &str) -> Self {
        with_global_session(|g| g.interner().intern(s))
    }

    pub fn as_str(&self) -> SymbolName {
        with_global_session(|g| g.interner().get(*self).clone())
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        with_global_session(|g| {
            f.debug_tuple("Symbol")
                .field(g.interner().get(*self))
                .finish()
        })
    }
}
