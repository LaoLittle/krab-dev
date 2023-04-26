use crate::symbol::Symbol;
use rustc_hash::FxHashMap;
use scoped_tls::scoped_thread_local;
use std::cell::RefCell;
use std::ops::{DerefMut, Range};

pub mod package;
pub mod symbol;

#[derive(Copy, Clone, Debug)]
pub struct Ident {
    symbol: Symbol,
    span: Span,
}

impl Ident {
    #[inline]
    pub const fn new(symbol: Symbol, span: Span) -> Self {
        Self { symbol, span }
    }

    pub const fn symbol(self) -> Symbol {
        self.symbol
    }

    #[inline]
    pub const fn span(self) -> Span {
        self.span
    }
}

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
        let mut i = Interner::new();
        i.prefill();
        Self {
            interner: RefCell::new(i),
        }
    }

    pub fn interner<'a>(&'a self) -> impl DerefMut<Target = Interner> + 'a {
        self.interner.borrow_mut()
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Span {
    pos: usize,
    len: u32,
}

impl Span {
    #[inline]
    pub const fn new(pos: usize, len: u32) -> Self {
        Self { pos, len }
    }

    pub const fn new_with_end(pos: usize, end: usize) -> Self {
        assert!(end >= pos);
        Self::new(pos, (end - pos) as u32)
    }

    #[inline]
    pub const fn pos(self) -> usize {
        self.pos
    }

    #[inline]
    pub const fn end(self) -> usize {
        self.pos + (self.len as usize)
    }

    #[inline]
    pub const fn range(self) -> Range<usize> {
        let Self { pos, len } = self;

        pos..pos + (len as usize)
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

macro_rules! pre_define_symbol {
    (Symbols { $($str:expr => $sym:ident: $num:expr);* $(;)? }) => {
        $(pub const $sym: $crate::symbol::Symbol = $crate::symbol::Symbol::new($num);)*

        fn _prefill(i: &mut Interner) {
            $(i.intern_static($str);)*
        }
    };
}

pre_define_symbol! {
    Symbols {
        "Boolean" => BOOLEAN: 0;
        "Int8" => INT8: 1;
        "Int16" => INT16: 2;
        "Int32" => INT32: 3;
        "Int64" => INT64: 4;
        "UInt8" => UINT8: 5;
        "UInt16" => UINT16: 6;
        "UInt32" => UINT32: 7;
        "UInt64" => UINT64: 8;
        "Unit" => UNIT: 9;
    }
}

impl Interner {
    pub fn new() -> Self {
        Self {
            name_map: FxHashMap::default(),
            strings: vec![],
        }
    }

    fn prefill(&mut self) {
        _prefill(self);
    }

    pub fn intern_static(&mut self, str: &'static str) -> Symbol {
        if let Some(&sym) = self.name_map.get(str) {
            return sym;
        }

        let sym = Symbol::new(self.strings.len() as u32);
        self.strings.push(str);
        self.name_map.insert(str, sym);

        sym
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
    use crate::{with_global_session_init, Interner, BOOLEAN, INT16, INT32, INT64, INT8};

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

    #[test]
    fn prefilled() {
        with_global_session_init(|| {
            assert_eq!(Symbol::intern("Boolean"), BOOLEAN);
            assert_eq!(Symbol::intern("Int8"), INT8);
            assert_eq!(Symbol::intern("Int16"), INT16);
            assert_eq!(Symbol::intern("Int32"), INT32);
            assert_eq!(Symbol::intern("Int64"), INT64);
        });
    }
}
