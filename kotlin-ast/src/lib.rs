use std::ops::Range;

pub mod decl;
pub mod expr;
pub mod stmt;

#[derive(Copy, Clone, Debug)]
pub struct Ident(Span);

impl Ident {
    #[inline]
    pub const fn new(start: usize, len: u32) -> Self {
        Self(Span::new(start, len))
    }

    #[inline]
    pub const fn new_with_end(start: usize, end: usize) -> Self {
        Self(Span::new_with_end(start, end))
    }

    #[inline]
    pub const fn span(self) -> Span {
        self.0
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Span {
    start: usize,
    len: u32,
}

impl Span {
    #[inline]
    pub const fn new(start: usize, len: u32) -> Self {
        Self { start, len }
    }

    pub const fn new_with_end(start: usize, end: usize) -> Self {
        assert!(end >= start);
        Self::new(start, (end - start) as u32)
    }

    #[inline]
    pub const fn start(self) -> usize {
        self.start
    }

    #[inline]
    pub const fn end(self) -> usize {
        self.start + (self.len as usize)
    }

    #[inline]
    pub const fn range(self) -> Range<usize> {
        let Self { start, len } = self;

        start..start + (len as usize)
    }
}
