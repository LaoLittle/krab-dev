use rustc_hash::FxHashMap;
use scoped_tls::scoped_thread_local;

pub mod symbol;

pub struct Span {}

struct Interner {
    name_map: FxHashMap<&'static str, u32>,
}
