use rustc_hash::FxHashMap;

pub struct Class {}

pub struct PackageMap {
    root: Vec<Class>,
    paths: FxHashMap<String, Vec<Class>>,
}
