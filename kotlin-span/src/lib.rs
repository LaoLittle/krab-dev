use crate::symbol::Symbol;
use rustc_hash::FxHashMap;
use scoped_tls::scoped_thread_local;
use std::cell::RefCell;
use std::ops::{DerefMut, Range};

pub mod package;
pub mod symbol;

scoped_thread_local!(static GLOBAL_SESSION: GlobalSession);

pub fn with_global_session_init<F: FnOnce() -> R, R>(f: F) -> R {
    assert!(
        !GLOBAL_SESSION.is_set(),
        "global session should not be overwrite"
    );
    let global = GlobalSession::new();
    GLOBAL_SESSION.set(&global, f)
}

pub fn with_global_session<F: FnOnce(&GlobalSession) -> R, R>(f: F) -> R {
    assert!(GLOBAL_SESSION.is_set());
    GLOBAL_SESSION.with(f)
}

pub struct GlobalSession {
    pub interner: RefCell<Interner>,
}

impl GlobalSession {
    pub fn new() -> Self {
        Self {
            interner: RefCell::new(Interner::new()),
        }
    }

    pub fn interner<'a>(&'a self) -> impl DerefMut<Target = Interner> + 'a {
        self.interner.borrow_mut()
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

    #[inline]
    pub fn str_slice(self, str: &str) -> &str {
        &str[self.range()]
    }
}

pub struct Interner {
    name_map: FxHashMap<&'static str, Symbol>,
    strings: Vec<&'static str>,
}

impl Interner {
    pub fn new() -> Self {
        Self {
            name_map: FxHashMap::default(),
            strings: vec![],
        }
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        if let Some(&sym) = self.name_map.get(s) {
            return sym;
        }

        let sym = Symbol::new(self.strings.len() as u32);
        let str: &'static [u8] = s.to_owned().into_bytes().leak();
        let str = std::str::from_utf8(str).unwrap();
        self.strings.push(str);
        self.name_map.insert(str, sym);

        sym
    }

    pub fn get(&self, symbol: Symbol) -> &'static str {
        self.strings[symbol.as_u32() as usize]
    }
}

#[cfg(test)]
mod tests {
    use crate::symbol::Symbol;
    use crate::Interner;

    #[test]
    fn symbol_intern() {
        let mut i = Interner::new();
        assert_eq!(i.intern("dog"), Symbol::new(0));
        assert_eq!(i.intern("dog"), Symbol::new(0));
        assert_eq!(i.intern("cat"), Symbol::new(1));
        assert_eq!(i.intern("dog"), Symbol::new(0));
        assert_eq!(i.intern("cat"), Symbol::new(1));
        assert_eq!(i.intern("foo"), Symbol::new(2));
        assert_eq!(i.intern("bar"), Symbol::new(3));
        assert_eq!(i.get(Symbol::new(0)), "dog");
        assert_eq!(i.get(Symbol::new(1)), "cat");
        assert_eq!(i.get(Symbol::new(2)), "foo");
        assert_eq!(i.get(Symbol::new(3)), "bar");
    }
}
