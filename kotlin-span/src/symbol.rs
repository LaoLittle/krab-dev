use crate::with_global_session;

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

impl Symbol {
    #[inline]
    pub const fn new(i: u32) -> Self {
        Self(i)
    }

    #[inline]
    pub const fn as_u32(self) -> u32 {
        self.0
    }

    pub fn intern(s: &str) -> Self {
        with_global_session(|g| g.interner().intern(s))
    }

    pub fn as_str(&self) -> &str {
        with_global_session(|g| g.interner().get(*self))
    }
}
