use std::{collections::HashMap, sync::Mutex};

use crate::types;
use lazy_static::lazy_static;

#[derive(Debug, Clone, PartialEq)]
pub enum StaticInit {
    CharInit(u8),
    IntInit(i64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum InitialValue {
    Tentative,
    Initial(Vec<StaticInit>),
    NoInitializer,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IdentifierAttrs {
    FunAttr { defined: bool, global: bool },
    StaticAttr { init: InitialValue, global: bool },
    ConstAttr(i64),
    LocalAttr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Entry {
    pub t: types::Type,
    pub attrs: IdentifierAttrs,
}

lazy_static! {
    static ref SYMBOL_TABLE: Mutex<HashMap<String, Entry>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

pub fn add_automatic_var(name: String, t: types::Type) {
    let mut map = SYMBOL_TABLE.lock().unwrap();
    map.insert(
        name,
        Entry {
            t,
            attrs: IdentifierAttrs::LocalAttr,
        },
    );
}

pub fn add_static_var(name: String, t: types::Type, global: bool, init: InitialValue) {
    let mut map = SYMBOL_TABLE.lock().unwrap();
    map.insert(
        name,
        Entry {
            t,
            attrs: IdentifierAttrs::StaticAttr { init, global },
        },
    );
}

pub fn add_fun(name: String, t: types::Type, global: bool, defined: bool) {
    let mut map = SYMBOL_TABLE.lock().unwrap();
    map.insert(
        name,
        Entry {
            t,
            attrs: IdentifierAttrs::FunAttr { defined, global },
        },
    );
}

pub fn get(name: String) -> Entry {
    let map = SYMBOL_TABLE.lock().unwrap();
    map.get(&name).unwrap().clone()
}

pub fn get_opt(name: String) -> Option<Entry> {
    let map = SYMBOL_TABLE.lock().unwrap();
    match map.get(&name) {
        Some(v) => Some(v.clone()),
        None => None,
    }
}

pub fn is_global(name: String) -> bool {
    match get(name).attrs {
        IdentifierAttrs::LocalAttr | IdentifierAttrs::ConstAttr(_) => false,
        IdentifierAttrs::StaticAttr { init: _, global } => global,
        IdentifierAttrs::FunAttr { defined: _, global } => global,
    }
}
