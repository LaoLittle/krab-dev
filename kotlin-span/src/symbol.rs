#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
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
}
