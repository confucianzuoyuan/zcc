use crate::tok::TokenIndex;

pub type ObjIndex = usize;

/// Variable or function
pub struct Obj {
    /// Variable name
    pub name: Option<String>,
    /// representative token
    pub tok: TokenIndex,
    /// local or global/function
    pub is_local: bool,
    /// alignment
    pub align: i32,
    /// Local variable
    pub offset: i32,
    /// Global variable or function
    pub is_function: bool,
    pub is_definition: bool,
    pub is_static: bool,
    /// Global variable
    pub is_tentative: bool,
    pub is_tls: bool,
    pub init_data: String,
    pub rel: Vec<Relocation>,

    /// Function
    pub is_inline: bool,
    pub params: Vec<ObjIndex>,
    pub locals: Vec<ObjIndex>,
    pub va_area: Vec<ObjIndex>,
    pub alloca_bottom: Vec<ObjIndex>,
    pub stack_size: i32,

    /// Static inline function
    pub is_live: bool,
    pub is_root: bool,
    pub refs: Vec<String>,
}

/// Global variable can be initialized either by a constant expression
/// or a pointer to another global variable. This struct represents the
/// latter.
pub struct Relocation {
    pub offset: i32,
    pub label: Vec<String>,
    pub addend: i64,
}
