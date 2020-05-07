use crate::common::IdxVec;
use crate::diagnostics::{Diagnostic, FileId, Label, Result, Span};
use crate::driver::Opts;
use crate::middle::ir;
use crate::ty;

use std::any::{Any, TypeId};
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::marker::PhantomData;
use std::ops::Deref;

use self::graph::{CallGraph, ControlGraph};
use self::map::{BlockMap, IrMap};

use super::{TyEnv, ENTRY_NAME};

mod graph;
mod map;
mod pp;

pub mod passes;
pub mod paths;
pub mod zones;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum DataFlowDir {
    Forward,
    Backward,
}

pub trait DataFlow: Sized + 'static {
    const DIR: DataFlowDir;

    type Context: Eq + fmt::Debug;
    type Summary: Eq + fmt::Debug;
    type Decoration: Clone + fmt::Debug;

    fn empty_context() -> Self::Context;

    fn empty_summary() -> Self::Summary;

    fn empty_decoration() -> Self::Decoration;

    fn entry(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        loc: AnalysisLoc,
        dec: &mut Self::Decoration,
    );

    fn expr(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        loc: AnalysisLoc,
        dec: &mut Self::Decoration,
        binding: ir::LocalIdx,
        expr: &ir::ExprKind,
    );

    fn pattern(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        loc: AnalysisLoc,
        dec: &mut Self::Decoration,
        source: ir::LocalIdx,
        pattern: &ir::PatternKind,
    );

    fn ret(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        loc: AnalysisLoc,
        dec: &mut Self::Decoration,
        idx: ir::LocalIdx,
    );

    fn conflate(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        loc: AnalysisLoc,
        decorations: Vec<Self::Decoration>,
    ) -> Self::Decoration;
}

pub struct AnalysisRuntime<'ir> {
    opts: &'ir Opts,
    ty_sess: &'ir ty::TySess,
    ty_env: &'ir TyEnv,
    ir: &'ir ir::Ir,
    entry: ir::DefIdx,
    cache: RefCell<HashMap<TypeId, Box<dyn Any>>>,
}

pub struct BlockDecoration<T>
where
    T: DataFlow,
{
    pub carry: T::Decoration,
    pub out: T::Decoration,
    pub instructions: Vec<T::Decoration>,
}

type LocalDecoration<T> = IdxVec<ir::BlockIdx, BlockDecoration<T>>;
type Decoration<T> = IdxVec<ir::DefIdx, LocalDecoration<T>>;

pub struct AnalysisEngine<'ir, T: DataFlow> {
    runtime: &'ir AnalysisRuntime<'ir>,
    ir_map: IrMap<'ir>,
    call_graph: CallGraph,
    contexts: RefCell<IdxVec<ir::DefIdx, T::Context>>,
    summaries: RefCell<IdxVec<ir::DefIdx, T::Summary>>,
    blocks: Decoration<T>,
    queue: RefCell<VecDeque<ir::DefIdx>>,
    _phantom: PhantomData<T>,
}

pub struct LocalAnalysisEngine<'ir, 'eng, T: DataFlow> {
    engine: &'eng AnalysisEngine<'ir, T>,
    def: ir::DefIdx,
    block_map: &'eng BlockMap<'ir>,
    block_queue: VecDeque<ir::BlockIdx>,
    blocks: LocalDecoration<T>,
    control_graph: &'eng ControlGraph,
}

pub struct Analysis<T: DataFlow> {
    contexts: IdxVec<ir::DefIdx, T::Context>,
    summaries: IdxVec<ir::DefIdx, T::Summary>,
    blocks: Decoration<T>,
}

impl<T: DataFlow> Analysis<T> {
    #[inline]
    pub fn decoration(&self, block: &ir::Block) -> &BlockDecoration<T> {
        &self.blocks[block.owner][block.block_idx]
    }

    #[inline]
    pub fn context(&self, idx: ir::DefIdx) -> &T::Context {
        &self.contexts[idx]
    }

    #[inline]
    pub fn summary(&self, idx: ir::DefIdx) -> &T::Summary {
        &self.summaries[idx]
    }

    pub fn lookup(&self, loc: AnalysisLoc) -> &T::Decoration {
        match loc.kind {
            AnalysisLocKind::Carry => &self.blocks[loc.def][loc.block].carry,
            AnalysisLocKind::Out => &self.blocks[loc.def][loc.block].out,
            AnalysisLocKind::Instruction(i) => &self.blocks[loc.def][loc.block].instructions[i],
        }
    }
}

impl<'ir> AnalysisRuntime<'ir> {
    fn new(
        opts: &'ir Opts,
        ty_sess: &'ir ty::TySess,
        ty_env: &'ir TyEnv,
        ir: &'ir ir::Ir,
        entry: ir::DefIdx,
    ) -> AnalysisRuntime<'ir> {
        AnalysisRuntime {
            opts,
            ty_sess,
            ty_env,
            ir,
            entry,
            cache: RefCell::new(HashMap::new()),
        }
    }

    pub fn pass<T: DataFlow>(&self) -> AnalysisRef<T> {
        if { self.cache.borrow().contains_key(&TypeId::of::<T>()) } {
            AnalysisRef {
                guard: self.cache.borrow(),
                _phantom: PhantomData::default(),
            }
        } else {
            let analysis = AnalysisEngine::<T>::new(self).compute();
            if self.opts.dump_analyses {
                use crate::pp;
                pp!(&(self.ir, &analysis));
            }
            self.cache
                .borrow_mut()
                .insert(TypeId::of::<T>(), Box::new(analysis) as Box<dyn Any>);
            self.pass::<T>()
        }
    }
}

impl<'ir, T: DataFlow> AnalysisEngine<'ir, T> {
    fn new(runtime: &'ir AnalysisRuntime) -> AnalysisEngine<'ir, T> {
        let mut contexts = IdxVec::with_capacity(runtime.ir.defs.len());
        let mut summaries = IdxVec::with_capacity(runtime.ir.defs.len());
        let mut blocks = IdxVec::with_capacity(runtime.ir.defs.len());
        for _ in runtime.ir.defs.keys() {
            contexts.push(T::empty_context());
            summaries.push(T::empty_summary());
            blocks.push(IdxVec::new());
        }
        AnalysisEngine {
            runtime,
            ir_map: IrMap::new(runtime.ir),
            call_graph: CallGraph::new(runtime.ir),
            contexts: RefCell::new(contexts),
            summaries: RefCell::new(summaries),
            blocks,
            queue: RefCell::new(runtime.ir.defs.keys().collect()),
            _phantom: PhantomData::default(),
        }
    }

    #[inline]
    fn ty_sess(&self) -> &ty::TySess {
        self.ty_sess
    }

    #[inline]
    fn ty_env(&self) -> &TyEnv {
        self.ty_env
    }

    #[inline]
    fn entry(&self) -> ir::DefIdx {
        self.entry
    }

    fn context(&self, def: ir::DefIdx) -> ContextRef<'_, T> {
        ContextRef {
            def,
            guard: self.contexts.borrow().into(),
        }
    }

    fn update_context(&self, def: ir::DefIdx, context: T::Context) {
        if context != self.contexts.borrow()[def] {
            debug!("updating context for {:?} to {:?}", def, context);
            let mut queue = self.queue.borrow_mut();
            if !queue.contains(&def) {
                debug!("queued {:?} for reanalysis!", def);
                queue.push_back(def);
            }
            self.contexts.borrow_mut()[def] = context
        }
    }

    fn summary(&self, def: ir::DefIdx) -> SummaryRef<'_, T> {
        SummaryRef {
            def,
            guard: self.summaries.borrow().into(),
        }
    }

    fn update_summary(&self, def: ir::DefIdx, summary: T::Summary) {
        if summary != self.summaries.borrow()[def] {
            debug!("updating summary for {:?} to {:?}", def, summary);
            let mut queue = self.queue.borrow_mut();
            for caller in self.call_graph.callers(def) {
                if !queue.contains(caller) {
                    debug!("queued {:?} for reanalysis!", caller);
                    queue.push_back(*caller);
                }
            }
            self.summaries.borrow_mut()[def] = summary
        }
    }

    fn execute(&mut self) {
        debug!("starting analysis pass...");
        loop {
            let next = { self.queue.borrow_mut().pop_front() };
            if let Some(def) = next {
                debug!("analysing {:?}...", def);
                self.blocks[def] = LocalAnalysisEngine::new(&self, &self.ir.defs[def]).compute();
            } else {
                break;
            }
        }
        debug!("completed analysis");
    }

    fn compute(mut self) -> Analysis<T> {
        self.execute();
        Analysis {
            contexts: self.contexts.into_inner(),
            summaries: self.summaries.into_inner(),
            blocks: self.blocks,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct AnalysisLoc {
    def: ir::DefIdx,
    block: ir::BlockIdx,
    kind: AnalysisLocKind,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum AnalysisLocKind {
    Carry,
    Out,
    Instruction(usize),
}

impl AnalysisLocKind {
    fn with_block(self, block: &ir::Block) -> AnalysisLoc {
        AnalysisLoc {
            def: block.owner,
            block: block.block_idx,
            kind: self,
        }
    }
}

impl<'ir, 'eng, T: DataFlow> LocalAnalysisEngine<'ir, 'eng, T> {
    fn new(
        engine: &'eng AnalysisEngine<'ir, T>,
        def: &'ir ir::Def,
    ) -> LocalAnalysisEngine<'ir, 'eng, T> {
        let control_graph = engine.call_graph.control_graph(def.def_idx);
        let block_map = engine.ir_map.blocks(def.def_idx);
        let block_queue = match T::DIR {
            DataFlowDir::Forward => vec![control_graph.entry()]
                .into_iter()
                .collect::<VecDeque<_>>(),
            DataFlowDir::Backward => control_graph.returns().copied().collect::<VecDeque<_>>(),
        };
        let mut blocks = IdxVec::new();
        for _ in 0..block_map.size() {
            // NOTE these feel like unecessary allocations and probably want removing
            blocks.push(BlockDecoration {
                carry: T::empty_decoration(),
                out: T::empty_decoration(),
                instructions: vec![],
            });
        }
        LocalAnalysisEngine {
            engine,
            def: def.def_idx,
            block_map,
            block_queue,
            blocks,
            control_graph,
        }
    }

    #[inline]
    fn def(&self) -> ir::DefIdx {
        self.def
    }

    #[inline]
    fn param_bindings(&self) -> &IdxVec<ty::ParamIdx, ir::LocalIdx> {
        &self.ir.defs[self.def].entry.param_bindings
    }

    fn forward_block(&mut self, block: &ir::Block) -> BlockDecoration<T> {
        let idx = block.block_idx;
        let carry = if let Some(pred) = self.control_graph.pred(idx) {
            let mut carry = self.blocks[pred].out.clone();
            if let ir::Terminator::Match { source, arms } = &self.block_map.lookup(pred).terminator
            {
                for arm in arms.iter() {
                    if arm.target.block_idx == idx {
                        T::pattern(
                            self,
                            AnalysisLocKind::Carry.with_block(block),
                            &mut carry,
                            *source,
                            &arm.pattern,
                        );
                    }
                }
            }
            carry
        } else {
            let mut initial = T::empty_decoration();
            T::entry(self, AnalysisLocKind::Carry.with_block(block), &mut initial);
            initial
        };
        let mut instructions: Vec<T::Decoration> = Vec::with_capacity(block.instructions.len());
        for (i, instruction) in block.instructions.iter().enumerate() {
            if let ir::InstructionKind::Let {
                binding,
                expr: ir::Expr { kind, .. },
                ..
            } = &instruction.kind
            {
                let mut prev = if i == 0 {
                    carry.clone()
                } else {
                    instructions[i - 1].clone()
                };
                T::expr(
                    self,
                    AnalysisLocKind::Instruction(i).with_block(block),
                    &mut prev,
                    *binding,
                    kind,
                );
                instructions.push(prev);
            }
        }
        let mut out = if instructions.is_empty() {
            carry.clone()
        } else {
            instructions[instructions.len() - 1].clone()
        };
        if let ir::Terminator::Return(ret) = &block.terminator {
            T::ret(self, AnalysisLocKind::Out.with_block(block), &mut out, *ret);
        } else {
            for succ in self.control_graph.succs(idx) {
                self.enque(*succ);
            }
        }
        BlockDecoration {
            carry,
            out,
            instructions,
        }
    }

    fn backward_block(&mut self, block: &ir::Block) -> BlockDecoration<T> {
        let idx = block.block_idx;
        let out = match &block.terminator {
            ir::Terminator::Return(ret) => {
                let mut initial = T::empty_decoration();
                T::ret(
                    self,
                    AnalysisLocKind::Out.with_block(block),
                    &mut initial,
                    *ret,
                );
                initial
            }
            ir::Terminator::Match { source, arms } => {
                let mut forwarded = vec![];
                for arm in arms.iter() {
                    let mut carry = self.blocks[arm.target.block_idx].carry.clone();
                    T::pattern(
                        self,
                        AnalysisLocKind::Carry.with_block(&arm.target),
                        &mut carry,
                        *source,
                        &arm.pattern,
                    );
                    forwarded.push(carry);
                }
                T::conflate(self, AnalysisLocKind::Out.with_block(block), forwarded)
            }
        };
        let mut carry = None;
        let mut instructions: Vec<T::Decoration> = Vec::with_capacity(block.instructions.len());
        instructions.push(out.clone());
        for (i, instruction) in block.instructions.iter().rev().enumerate() {
            if let ir::InstructionKind::Let {
                binding,
                expr: ir::Expr { kind, .. },
                ..
            } = &instruction.kind
            {
                let mut prev = instructions[i].clone();
                T::expr(
                    self,
                    AnalysisLocKind::Instruction(block.instructions.len() - i - 1)
                        .with_block(block),
                    &mut prev,
                    *binding,
                    kind,
                );
                if i < block.instructions.len() - 1 {
                    instructions.push(prev);
                } else {
                    carry = Some(prev);
                }
            }
        }
        instructions.reverse();
        let mut carry = carry.unwrap_or_else(|| out.clone());
        if let Some(pred) = self.control_graph.pred(idx) {
            self.enque(pred);
        } else {
            T::entry(self, AnalysisLocKind::Carry.with_block(block), &mut carry);
        }
        BlockDecoration {
            carry,
            out,
            instructions,
        }
    }

    fn enque(&mut self, idx: ir::BlockIdx) {
        if !self.block_queue.contains(&idx) {
            self.block_queue.push_back(idx);
        }
    }

    fn execute(&mut self) {
        while let Some(idx) = self.block_queue.pop_front() {
            debug!("analysing {:?}...", idx);
            let block = self.block_map.lookup(idx);
            self.blocks[idx] = match T::DIR {
                DataFlowDir::Forward => self.forward_block(block),
                DataFlowDir::Backward => self.backward_block(block),
            };
        }
    }

    fn compute(mut self) -> LocalDecoration<T> {
        self.execute();
        self.blocks
    }
}

impl<'ir, 'eng, T: DataFlow> Deref for LocalAnalysisEngine<'ir, 'eng, T> {
    type Target = AnalysisEngine<'ir, T>;

    fn deref(&self) -> &Self::Target {
        self.engine
    }
}

impl<'ir, T: DataFlow> Deref for AnalysisEngine<'ir, T> {
    type Target = AnalysisRuntime<'ir>;

    fn deref(&self) -> &Self::Target {
        self.runtime
    }
}

pub struct ContextRef<'a, T: DataFlow> {
    def: ir::DefIdx,
    guard: Ref<'a, IdxVec<ir::DefIdx, T::Context>>,
}

pub struct SummaryRef<'a, T: DataFlow> {
    def: ir::DefIdx,
    guard: Ref<'a, IdxVec<ir::DefIdx, T::Summary>>,
}

pub struct AnalysisRef<'a, T: DataFlow> {
    guard: Ref<'a, HashMap<TypeId, Box<dyn Any>>>,
    _phantom: PhantomData<T>,
}

impl<'a, T: DataFlow> Deref for ContextRef<'a, T> {
    type Target = T::Context;

    fn deref(&self) -> &Self::Target {
        self.guard.get(self.def).unwrap()
    }
}

impl<'a, T: DataFlow> Deref for SummaryRef<'a, T> {
    type Target = T::Summary;

    fn deref(&self) -> &Self::Target {
        self.guard.get(self.def).unwrap()
    }
}

impl<'a, T: DataFlow> Deref for AnalysisRef<'a, T> {
    type Target = Analysis<T>;

    fn deref(&self) -> &Self::Target {
        self.guard
            .get(&TypeId::of::<T>())
            .and_then(|boxed| boxed.downcast_ref::<Analysis<T>>())
            .unwrap()
    }
}

impl ir::Ir {
    fn entry(&self) -> Option<ir::DefIdx> {
        for (idx, def) in self.defs.iter() {
            if def.name == ENTRY_NAME {
                return Some(idx);
            }
        }
        None
    }
}

pub fn runtime<'ir>(
    opts: &'ir Opts,
    file_id: FileId,
    ty_sess: &'ir ty::TySess,
    ty_env: &'ir TyEnv,
    ir: &'ir ir::Ir,
) -> Result<AnalysisRuntime<'ir>> {
    let entry = ir.entry().ok_or_else(|| {
        Diagnostic::new_error(
            "no entry point",
            Label::new(
                file_id,
                Span::dummy(),
                &format!("no function with name '{}' found", ENTRY_NAME),
            ),
        )
    })?;
    Ok(AnalysisRuntime::new(opts, ty_sess, ty_env, ir, entry))
}
