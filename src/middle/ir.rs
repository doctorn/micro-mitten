use crate::common::{Idx, IdxVec, Idxr};
use crate::diagnostics::Span;
use crate::ty::{FieldIdx, ParamIdx, Ty, VariantIdx};

use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct DefIdx(usize);

impl fmt::Debug for DefIdx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ".{}", self.0)
    }
}

impl Idx for DefIdx {
    #[inline]
    fn index(&self) -> usize {
        self.0
    }

    #[inline]
    fn new(index: usize) -> Self {
        DefIdx(index)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BlockIdx(usize);

impl Idx for BlockIdx {
    #[inline]
    fn index(&self) -> usize {
        self.0
    }

    #[inline]
    fn new(index: usize) -> Self {
        BlockIdx(index)
    }
}

#[derive(Copy, Clone, Eq)]
pub struct LocalIdx {
    span: Span,
    idx: usize,
}

impl fmt::Debug for LocalIdx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.idx)
    }
}

impl LocalIdx {
    pub fn with_span(&self, span: Span) -> LocalIdx {
        LocalIdx {
            span,
            idx: self.idx,
        }
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }
}

impl Idx for LocalIdx {
    #[inline]
    fn index(&self) -> usize {
        self.idx
    }

    fn new(index: usize) -> Self {
        LocalIdx {
            span: Span::dummy(),
            idx: index,
        }
    }
}

impl PartialEq for LocalIdx {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl Hash for LocalIdx {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.idx.hash(state)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnopKind {
    Not,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinopKind {
    Plus,
    Minus,
    Mul,
    Div,
    Less,
    Leq,
    Greater,
    Geq,
    Eq,
    Neq,
    And,
    Or,
    Xor,
    LShift,
    RShift,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Literal(u64),
    Var(LocalIdx),
    Unop {
        kind: UnopKind,
        operand: LocalIdx,
    },
    Binop {
        kind: BinopKind,
        left: LocalIdx,
        right: LocalIdx,
    },
    Call {
        target: DefIdx,
        args: IdxVec<ParamIdx, LocalIdx>,
    },
    Variant {
        ty: Ty,
        discriminant: VariantIdx,
        body: LocalIdx,
    },
    Record {
        ty: Ty,
        fields: IdxVec<FieldIdx, LocalIdx>,
    },
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub local_idx: LocalIdx,
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Clone, Debug)]
pub enum PatternKind {
    Literal(u64),
    Ident(LocalIdx),
    Variant {
        ty: Ty,
        discriminant: VariantIdx,
        binding: LocalIdx,
    },
    Record {
        ty: Ty,
        fields: IdxVec<FieldIdx, LocalIdx>,
    },
}

#[derive(Debug)]
pub struct Arm {
    pub span: Span,
    pub pattern: PatternKind,
    pub target: Box<Block>,
}

#[derive(Clone, Debug)]
pub enum InstructionKind {
    Let {
        binding: LocalIdx,
        ty: Option<Ty>,
        expr: Expr,
    },
    Mark(LocalIdx, Ty),
    Unmark(LocalIdx, Ty),
    Free(LocalIdx, Ty),
    RTReset,
}

#[derive(Clone, Debug)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum Terminator {
    Return(LocalIdx),
    Match { source: LocalIdx, arms: Vec<Arm> },
}

#[derive(Debug)]
pub struct Block {
    pub owner: DefIdx,
    pub block_idx: BlockIdx,
    pub span: Span,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(Debug)]
pub struct Entry {
    pub owner: DefIdx,
    pub param_bindings: IdxVec<ParamIdx, LocalIdx>,
    pub body: Block,
}

#[derive(Debug)]
pub struct Def {
    pub def_idx: DefIdx,
    pub name: String,
    pub ty: Ty,
    pub span: Span,
    pub entry: Entry,
    pub local_idxr: Idxr<LocalIdx>,
}

#[derive(Debug)]
pub struct Ir {
    pub defs: IdxVec<DefIdx, Def>,
}
