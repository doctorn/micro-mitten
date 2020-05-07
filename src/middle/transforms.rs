use crate::common::{IdxVec, Idxr, Map};
use crate::diagnostics::{FileId, Result, Span};
use crate::driver::Opts;
use crate::middle::analysis::passes::{Access, AccessDecoration, ImpliedAccess, ShapeDecoration};
use crate::middle::analysis::paths::Path;
use crate::middle::analysis::zones::Zone;
use crate::middle::analysis::{self, AnalysisRef, AnalysisRuntime};
use crate::middle::ir;
use crate::middle::ty_check;
use crate::middle::ty_check::TyEnv;
use crate::pp;
use crate::ty;

use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;

macro_rules! mtn_call {
    ( $target:expr, $( $arg:expr ),* ) => {
        ir::ExprKind::Call {
            target: $target,
            args: {
                let mut args = IdxVec::new();
                $( args.push($arg); )*
                args
            },
        }
    };
}

macro_rules! mtn_mark {
    ( $idx:expr, $ty:expr ) => {
        ir::Instruction {
            kind: ir::InstructionKind::Mark($idx, $ty),
            span: Span::dummy(),
        }
    };
}

macro_rules! mtn_unmark {
    ( $idx:expr, $ty:expr ) => {
        ir::Instruction {
            kind: ir::InstructionKind::Unmark($idx, $ty),
            span: Span::dummy(),
        }
    };
}

macro_rules! mtn_free {
    ( $idx:expr, $ty:expr ) => {
        ir::Instruction {
            kind: ir::InstructionKind::Free($idx, $ty),
            span: Span::dummy(),
        }
    };
}

macro_rules! mtn_let {
    ( $idx:expr, $expr:expr ) => {{
        let idx = $idx;
        ir::Instruction {
            kind: ir::InstructionKind::Let {
                binding: idx,
                ty: None,
                expr: ir::Expr {
                    local_idx: idx,
                    span: Span::dummy(),
                    kind: $expr,
                },
            },
            span: Span::dummy(),
        }
    }};
}

macro_rules! mtn_arm {
    ( $pattern:expr => $target:expr ) => {
        ir::Arm {
            span: Span::dummy(),
            pattern: $pattern,
            target: Box::new($target),
        }
    };
}

macro_rules! mtn_match {
    ( $src:expr, $( $arm:expr )* ) => {
        ir::Terminator::Match {
            source: $src,
            arms: vec![$( $arm, )*]
        }
    }
}

macro_rules! mtn_ret {
    ( $idx:expr ) => {
        ir::Terminator::Return($idx)
    };
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum ScanMode {
    Mark,
    Unmark,
    Free,
}

type ScanGroup = RefCell<HashMap<ty::Ty, ir::DefIdx>>;

struct CleanCtx<'ir> {
    ty_sess: &'ir ty::TySess,
    runtime: &'ir AnalysisRuntime<'ir>,
    uniqued: HashMap<ScanMode, RefCell<Map<Path, ScanGroup>>>,
    defs: RefCell<IdxVec<ir::DefIdx, Option<ir::Def>>>,
}

struct LocalCleanCtx<'ir, 'ctx> {
    ctx: &'ctx CleanCtx<'ir>,
    def: &'ir ir::Def,
    local_idxr: Idxr<ir::LocalIdx>,
}

struct BlockCleanCtx<'ir, 'ctx> {
    ctx: &'ctx LocalCleanCtx<'ir, 'ctx>,
    block: &'ir ir::Block,
    instructions: Vec<ir::Instruction>,
}

struct ScanBuilder<'ir, 'ctx> {
    ctx: &'ctx CleanCtx<'ir>,
    mode: ScanMode,
    ty: ty::Ty,
    def_idx: ir::DefIdx,
    block_idxr: Idxr<ir::BlockIdx>,
    local_idxr: Idxr<ir::LocalIdx>,
}

impl<'ir> CleanCtx<'ir> {
    fn new(
        ty_sess: &'ir ty::TySess,
        runtime: &'ir AnalysisRuntime<'ir>,
        ir: &'ir ir::Ir,
    ) -> CleanCtx<'ir> {
        let mut uniqued = HashMap::new();
        uniqued.insert(ScanMode::Mark, RefCell::new(Map::new()));
        uniqued.insert(ScanMode::Unmark, RefCell::new(Map::new()));
        uniqued.insert(ScanMode::Free, RefCell::new(Map::new()));
        let mut defs = IdxVec::new();
        for _ in ir.defs.iter() {
            defs.push(None);
        }
        CleanCtx {
            ty_sess,
            runtime,
            uniqued,
            defs: RefCell::new(defs),
        }
    }

    fn protected(
        &self,
        anti_matter: &AccessDecoration<ir::LocalIdx>,
        instruction: &'ir ir::Instruction,
    ) -> AccessDecoration<ir::LocalIdx> {
        let mut protected = AccessDecoration::new();
        if let ir::InstructionKind::Let {
            expr:
                ir::Expr {
                    kind: ir::ExprKind::Call { target, args },
                    ..
                },
            ..
        } = &instruction.kind
        {
            let context = self.access().context(*target).1.clone();
            for zone in anti_matter.zones() {
                let mut flagged = false;
                for (param, idx) in args.iter() {
                    if zone.idx() == *idx && !context.accessed_on(&zone.clone().with_idx(param)) {
                        flagged = true;
                    }
                }
                if flagged {
                    protected.add(zone);
                }
            }
        }
        protected
    }

    fn scan_zone(
        &self,
        mode: ScanMode,
        ty: ty::Ty,
        idx: ir::LocalIdx,
        zone: &Zone<ir::LocalIdx>,
    ) -> ir::ExprKind {
        let uniqued = self.uniqued.get(&mode).unwrap();
        {
            if let Some(grouping) = uniqued.borrow().get(zone.path()) {
                {
                    if let Some(def_idx) = grouping.borrow().get(&ty) {
                        return mtn_call!(*def_idx, idx);
                    }
                }
                let def_idx = { self.defs.borrow_mut().push(None) };
                {
                    grouping.borrow_mut().insert(ty, def_idx);
                }
                let def = ScanBuilder::new(&self, mode, def_idx, ty).scan(zone);
                {
                    self.defs.borrow_mut()[def_idx] = Some(def);
                }
                return mtn_call!(def_idx, idx);
            }
        }
        {
            uniqued
                .borrow_mut()
                .insert(zone.path().clone(), RefCell::new(HashMap::new()));
        }
        self.scan_zone(mode, ty, idx, zone)
    }

    fn emit_scan(
        &self,
        mode: ScanMode,
        ty: ty::Ty,
        idx: ir::LocalIdx,
        zone: &Zone<ir::LocalIdx>,
        local_idxr: &Idxr<ir::LocalIdx>,
        instructions: &mut Vec<ir::Instruction>,
    ) {
        if ty == self.ty_sess.mk_u64() {
        } else if zone.path().is_empty() {
            instructions.push(match mode {
                ScanMode::Mark => mtn_mark!(idx, ty),
                ScanMode::Unmark => mtn_unmark!(idx, ty),
                ScanMode::Free => mtn_free!(idx, ty),
            });
        } else {
            instructions.push(mtn_let!(
                local_idxr.next(),
                self.scan_zone(mode, ty, idx, zone)
            ));
        }
    }

    #[inline]
    fn access(&self) -> AnalysisRef<'_, Access> {
        self.runtime.pass::<Access>()
    }

    #[inline]
    fn implied(&self) -> AnalysisRef<'_, ImpliedAccess> {
        self.runtime.pass::<ImpliedAccess>()
    }

    fn clean(self, ir: &'ir ir::Ir) -> ir::Ir {
        let mut defs = IdxVec::new();
        for def in ir.defs.values() {
            let cleaned_def = LocalCleanCtx::new(&self, def).clean();
            defs.push(cleaned_def);
        }
        for (idx, def) in self.defs.into_inner().into_iter().skip(ir.defs.len()) {
            let def_idx = defs.push(def.expect("unconstructed scanning function"));
            debug_assert_eq!(idx, def_idx);
        }
        ir::Ir { defs }
    }
}

impl<'ir, 'ctx> LocalCleanCtx<'ir, 'ctx> {
    fn new(ctx: &'ctx CleanCtx<'ir>, def: &'ir ir::Def) -> LocalCleanCtx<'ir, 'ctx> {
        LocalCleanCtx {
            ctx,
            def,
            local_idxr: def.local_idxr.clone(),
        }
    }

    fn clean_terminator(&self, block: &'ir ir::Block) -> ir::Terminator {
        match &block.terminator {
            ir::Terminator::Match { source, arms } => {
                let mut cleaned_arms = vec![];
                let pre_access = self.access().decoration(block).out.clone();
                for arm in arms.iter() {
                    let block = self.clean_block_with_preaccess(
                        &arm.target,
                        &pre_access,
                        *source,
                        &arm.pattern,
                    );
                    cleaned_arms.push(mtn_arm!(arm.pattern.clone() => block));
                }
                ir::Terminator::Match {
                    source: *source,
                    arms: cleaned_arms,
                }
            }
            ir::Terminator::Return(idx) => mtn_ret!(*idx),
        }
    }

    fn clean_block_with_preaccess(
        &self,
        block: &'ir ir::Block,
        pre_access: &AccessDecoration<ir::LocalIdx>,
        source: ir::LocalIdx,
        pattern: &'ir ir::PatternKind,
    ) -> ir::Block {
        let mut block_ctx = BlockCleanCtx::new(&self, block);
        let post_access = self.access().decoration(block).carry.clone();
        let implied = self.implied().decoration(block).carry.clone();
        match pattern {
            ir::PatternKind::Variant {
                ty, discriminant, ..
            } => {
                block_ctx.process_clean(
                    &implied,
                    pre_access,
                    &post_access,
                    |zone| {
                        zone.idx() != source
                            || zone.path().contains_variant_edge(*ty, *discriminant)
                    },
                    |_| AccessDecoration::new(),
                );
            }
            _ => {
                block_ctx.process_clean(
                    &implied,
                    pre_access,
                    &post_access,
                    |_| true,
                    |_| AccessDecoration::new(),
                );
            }
        }
        block_ctx.clean()
    }

    fn clean_block(&self, block: &'ir ir::Block) -> ir::Block {
        BlockCleanCtx::new(&self, block).clean()
    }

    fn clean_entry(&self, entry: &'ir ir::Entry) -> ir::Entry {
        ir::Entry {
            owner: self.def.def_idx,
            param_bindings: entry.param_bindings.clone(),
            body: self.clean_block(&entry.body),
        }
    }

    fn clean(self) -> ir::Def {
        ir::Def {
            def_idx: self.def.def_idx,
            name: self.def.name.clone(),
            ty: self.def.ty,
            span: Span::dummy(),
            entry: self.clean_entry(&self.def.entry),
            local_idxr: self.local_idxr,
        }
    }
}

impl<'ir, 'ctx> BlockCleanCtx<'ir, 'ctx> {
    fn new(ctx: &'ctx LocalCleanCtx<'ir, 'ctx>, block: &'ir ir::Block) -> BlockCleanCtx<'ir, 'ctx> {
        BlockCleanCtx {
            ctx,
            block,
            instructions: vec![],
        }
    }

    fn emit_scan(&mut self, mode: ScanMode, zone: &Zone<ir::LocalIdx>) {
        self.ctx.emit_scan(
            mode,
            zone.path().src,
            zone.idx(),
            zone,
            &self.ctx.local_idxr,
            &mut self.instructions,
        );
    }

    fn emit_clean(
        &mut self,
        matter: &AccessDecoration<ir::LocalIdx>,
        anti_matter: &AccessDecoration<ir::LocalIdx>,
    ) {
        // mark matter
        for zone in matter.zones() {
            self.emit_scan(ScanMode::Mark, &zone);
        }
        // free anti-matter
        for zone in anti_matter.zones() {
            self.emit_scan(ScanMode::Free, &zone);
        }
        // unmark matter
        for zone in matter.zones() {
            self.emit_scan(ScanMode::Unmark, &zone);
        }
        // reset runtime
        self.instructions.push(ir::Instruction {
            kind: ir::InstructionKind::RTReset,
            span: Span::dummy(),
        })
    }

    fn process_clean(
        &mut self,
        implied: &ShapeDecoration<ir::LocalIdx>,
        pre_access: &AccessDecoration<ir::LocalIdx>,
        post_access: &AccessDecoration<ir::LocalIdx>,
        matter_filter: impl Fn(&Zone<ir::LocalIdx>) -> bool,
        protected: impl FnOnce(&AccessDecoration<ir::LocalIdx>) -> AccessDecoration<ir::LocalIdx>,
    ) {
        if !pre_access.is_empty() {
            let mut anti_matter = pre_access.anti_matter_with(self.ty_sess, post_access);
            let protected = protected(&anti_matter);
            if !protected.is_empty() {
                let mut culled = AccessDecoration::new();
                for zone in anti_matter
                    .zones()
                    .filter(|zone| !protected.accessed_on(zone))
                {
                    if !protected
                        .zones()
                        .any(|protected| implied.shapes(&zone, &protected))
                    {
                        culled.add(zone);
                    }
                }
                anti_matter = culled;
            }
            if !anti_matter.is_empty() {
                let matter =
                    post_access.matter_with(self.ty_sess, &anti_matter, implied, matter_filter);
                self.emit_clean(&matter, &anti_matter);
            }
        }
    }

    fn clean_instructions(&mut self) {
        let mut pre_access;
        let mut post_access = self.access().decoration(&self.block).carry.clone();
        for (i, instruction) in self.block.instructions.iter().enumerate() {
            self.instructions.push(instruction.clone());
            pre_access = post_access;
            post_access = self.access().decoration(&self.block).instructions[i].clone();
            let implied = self.implied().decoration(&self.block).instructions[i].clone();
            let ctx = self.ctx;
            self.process_clean(
                &implied,
                &pre_access,
                &post_access,
                |_| true,
                move |anti_matter| ctx.protected(anti_matter, instruction),
            );
        }
    }

    fn clean(mut self) -> ir::Block {
        self.clean_instructions();
        let terminator = self.clean_terminator(self.block);
        ir::Block {
            owner: self.block.owner,
            block_idx: self.block.block_idx,
            span: self.block.span,
            instructions: self.instructions,
            terminator,
        }
    }
}

impl<'ir, 'ctx> ScanBuilder<'ir, 'ctx> {
    fn new(
        ctx: &'ctx CleanCtx<'ir>,
        mode: ScanMode,
        def_idx: ir::DefIdx,
        ty: ty::Ty,
    ) -> ScanBuilder<'ir, 'ctx> {
        ScanBuilder {
            ctx,
            mode,
            def_idx,
            block_idxr: Idxr::new(),
            local_idxr: Idxr::new(),
            ty,
        }
    }

    fn build_ty(&self) -> ty::Ty {
        self.ctx.ty_sess.mk_fn(self.ctx.ty_sess.mk_u64(), {
            let mut params = IdxVec::new();
            params.push(self.ty);
            params
        })
    }

    fn build_param_bindings(&self) -> (IdxVec<ty::ParamIdx, ir::LocalIdx>, ir::LocalIdx) {
        let mut param_bindings = IdxVec::new();
        let arg = self.local_idxr.next();
        param_bindings.push(arg);
        (param_bindings, arg)
    }

    fn build_return(&self, instructions: &mut Vec<ir::Instruction>) -> ir::Terminator {
        let zero = self.local_idxr.next();
        instructions.push(mtn_let!(zero, ir::ExprKind::Literal(0)));
        mtn_ret!(zero)
    }

    fn build_struct_terminator(
        &self,
        arg: ir::LocalIdx,
        zone: &Zone<ir::LocalIdx>,
    ) -> ir::Terminator {
        let mut fields = IdxVec::new();
        for _ in 0..self.ctx.ty_sess.ty_kind(self.ty).field_count().unwrap() {
            fields.push(self.local_idxr.next());
        }
        let mut instructions = vec![];
        for (field, idx) in fields.iter() {
            if zone.path().contains_field_edge(self.ty, field) {
                let target = { self.ctx.ty_sess.ty_kind(self.ty).field_ty(field).unwrap() };
                self.ctx.emit_scan(
                    self.mode,
                    target,
                    *idx,
                    zone,
                    &self.local_idxr,
                    &mut instructions,
                );
            }
        }
        let terminator = self.build_return(&mut instructions);
        let block = ir::Block {
            owner: self.def_idx,
            block_idx: self.block_idxr.next(),
            span: Span::dummy(),
            instructions,
            terminator,
        };
        mtn_match!(
            arg,
            mtn_arm!(ir::PatternKind::Record { ty: self.ty, fields } => block)
        )
    }

    fn build_enum_terminator(
        &self,
        arg: ir::LocalIdx,
        zone: &Zone<ir::LocalIdx>,
    ) -> Option<ir::Terminator> {
        let mut arms = vec![];
        let variant_count = { self.ctx.ty_sess.ty_kind(self.ty).variant_count().unwrap() };
        for discriminant in 0..variant_count {
            use crate::common::Idx;
            let discriminant = ty::VariantIdx::new(discriminant);
            if zone.path().contains_variant_edge(self.ty, discriminant) {
                let binding = self.local_idxr.next();
                let mut instructions = vec![];
                let target = {
                    self.ctx
                        .ty_sess
                        .ty_kind(self.ty)
                        .variant_ty(discriminant)
                        .unwrap()
                };
                self.ctx.emit_scan(
                    self.mode,
                    target,
                    binding,
                    zone,
                    &self.local_idxr,
                    &mut instructions,
                );
                let block = ir::Block {
                    owner: self.def_idx,
                    block_idx: self.block_idxr.next(),
                    span: Span::dummy(),
                    terminator: self.build_return(&mut instructions),
                    instructions,
                };
                arms.push(mtn_arm!(
                    ir::PatternKind::Variant {
                        ty: self.ty,
                        discriminant,
                        binding,
                    } => block
                ))
            }
        }
        if arms.is_empty() {
            None
        } else {
            if arms.len() != variant_count {
                let mut instructions = vec![];
                let block = ir::Block {
                    owner: self.def_idx,
                    block_idx: self.block_idxr.next(),
                    span: Span::dummy(),
                    terminator: self.build_return(&mut instructions),
                    instructions,
                };
                arms.push(mtn_arm!(ir::PatternKind::Ident(self.local_idxr.next()) => block))
            }
            Some(ir::Terminator::Match { source: arg, arms })
        }
    }

    fn build_terminator(
        &self,
        arg: ir::LocalIdx,
        zone: &Zone<ir::LocalIdx>,
    ) -> Option<ir::Terminator> {
        if self.ctx.ty_sess.ty_kind(self.ty).is_struct() {
            Some(self.build_struct_terminator(arg, zone))
        } else if self.ctx.ty_sess.ty_kind(self.ty).is_enum() {
            self.build_enum_terminator(arg, zone)
        } else {
            None
        }
    }

    fn build_body(&self, arg: ir::LocalIdx, zone: &Zone<ir::LocalIdx>) -> ir::Block {
        let mut instructions = match self.mode {
            ScanMode::Mark => vec![mtn_mark!(arg, self.ty)],
            ScanMode::Unmark => vec![mtn_unmark!(arg, self.ty)],
            ScanMode::Free if self.ty == zone.path().dest => vec![mtn_free!(arg, self.ty)],
            _ => vec![],
        };
        let terminator = self
            .build_terminator(arg, zone)
            .unwrap_or_else(|| self.build_return(&mut instructions));
        ir::Block {
            owner: self.def_idx,
            block_idx: self.block_idxr.next(),
            span: Span::dummy(),
            instructions,
            terminator,
        }
    }

    fn build_entry(&self, zone: &Zone<ir::LocalIdx>) -> ir::Entry {
        let (param_bindings, arg) = self.build_param_bindings();
        ir::Entry {
            owner: self.def_idx,
            param_bindings,
            body: self.build_body(arg, zone),
        }
    }

    fn scan(self, zone: &Zone<ir::LocalIdx>) -> ir::Def {
        ir::Def {
            def_idx: self.def_idx,
            name: format!("__scan{:?}", self.def_idx),
            span: Span::dummy(),
            entry: self.build_entry(zone),
            ty: self.build_ty(),
            local_idxr: self.local_idxr,
        }
    }
}

impl<'ir, 'ctx> Deref for LocalCleanCtx<'ir, 'ctx> {
    type Target = CleanCtx<'ir>;

    fn deref(&self) -> &Self::Target {
        self.ctx
    }
}

impl<'ir, 'ctx> Deref for BlockCleanCtx<'ir, 'ctx> {
    type Target = LocalCleanCtx<'ir, 'ctx>;

    fn deref(&self) -> &Self::Target {
        self.ctx
    }
}

pub fn clean(
    opts: &Opts,
    file_id: FileId,
    ty_sess: &ty::TySess,
    ty_env: &TyEnv,
    ir: &ir::Ir,
) -> Result<ir::Ir> {
    let runtime = analysis::runtime(opts, file_id, ty_sess, ty_env, ir)?;
    let cleaned = CleanCtx::new(ty_sess, &runtime, ir).clean(ir);
    if opts.dump_ir {
        pp!(&cleaned);
    }
    ty_check(opts, file_id, ty_sess, &cleaned)?;
    Ok(cleaned)
}
