mod lowering;
mod pp;
mod ty_check;

pub mod analysis;
pub mod ir;
pub mod transforms;

pub const ENTRY_NAME: &str = "main";
pub const RESERVED_NAMES: &[&str] = &[
    "malloc",
    "GC_malloc",
    "free",
    "mitten_reset",
    "mitten_free",
    "mitten_alloc",
];

pub use analysis::runtime;
pub use lowering::lower;
pub use ty_check::{ty_check, TyEnv};
