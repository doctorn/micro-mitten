use crate::common::{IdxVec, IntoIdxVec};
use crate::diagnostics::{Diagnostic, FileId, Label, Result, Span};
use crate::driver::Opts;
use crate::middle::ir;
use crate::ty;

use std::collections::HashMap;
use std::ops::Deref;

pub type TyEnv = IdxVec<ir::DefIdx, IdxVec<ir::LocalIdx, ty::Ty>>;

struct TyCtx<'tcx> {
    #[allow(unused)]
    sess: &'tcx Opts,
    file_id: FileId,
    ty_sess: &'tcx ty::TySess,
    tys: HashMap<ir::DefIdx, (Span, ty::Ty)>,
}

impl<'tcx> TyCtx<'tcx> {
    fn bind(&mut self, idx: ir::DefIdx, ty: ty::Ty, span: Span) -> Result<()> {
        if let Some((other_span, other_ty)) = self.tys.insert(idx, (span, ty)) {
            return Err(Diagnostic::new_error(
                "could not infer single type for value",
                Label::new(
                    self.file_id,
                    span,
                    &format!("attempted to rebind as {:?}", ty),
                ),
            )
            .with_secondary_labels(vec![Label::new(
                self.file_id,
                other_span,
                &format!("previously bound as {:?}", other_ty),
            )]));
        }
        Ok(())
    }

    fn lookup(&self, idx: ir::DefIdx, span: Span) -> Result<ty::Ty> {
        if let Some((_, ty)) = self.tys.get(&idx) {
            Ok(*ty)
        } else {
            Err(Diagnostic::new_error(
                "reference to unbound definition",
                Label::new(self.file_id, span, "not bound in this scope"),
            ))
        }
    }

    fn check_ir(
        sess: &'tcx Opts,
        file_id: FileId,
        ty_sess: &'tcx ty::TySess,
        ir: &'tcx ir::Ir,
    ) -> Result<TyEnv> {
        let mut global_ctx = TyCtx {
            sess,
            file_id,
            ty_sess,
            tys: HashMap::new(),
        };
        for (def_idx, def) in ir.defs.iter() {
            global_ctx.bind(def_idx, def.ty, def.span)?;
        }
        let mut env = IdxVec::new();
        for def in ir.defs.values() {
            env.push(LocalTyCtx::check_def(&global_ctx, def)?);
        }
        Ok(env)
    }
}

struct LocalTyCtx<'tcx> {
    global_ctx: &'tcx TyCtx<'tcx>,
    prototype: ty::Prototype,
    tys: HashMap<ir::LocalIdx, (Span, ty::Ty)>,
}

impl<'tcx> LocalTyCtx<'tcx> {
    fn bind(&mut self, idx: ir::LocalIdx, ty: ty::Ty) -> Result<()> {
        if let Some((other_span, other_ty)) = self.tys.insert(idx, (idx.span(), ty)) {
            return Err(Diagnostic::new_error(
                "could not infer single type for value",
                Label::new(
                    self.file_id,
                    idx.span(),
                    &format!("attempted to rebind as {:?}", ty),
                ),
            )
            .with_secondary_labels(vec![Label::new(
                self.file_id,
                other_span,
                &format!("previously bound as {:?}", other_ty),
            )]));
        }
        Ok(())
    }

    fn lookup(&self, idx: ir::LocalIdx) -> Result<ty::Ty> {
        if let Some((_, ty)) = self.tys.get(&idx) {
            Ok(*ty)
        } else {
            Err(Diagnostic::new_error(
                "reference to unbound variable",
                Label::new(
                    self.file_id,
                    idx.span(),
                    "not bound in this scope (while type checking)",
                ),
            ))
        }
    }

    fn check_expr_kind(&mut self, expr_kind: &ir::ExprKind, span: Span) -> Result<ty::Ty> {
        match expr_kind {
            ir::ExprKind::Literal(_) => Ok(self.ty_sess.mk_u64()),
            ir::ExprKind::Var(local_idx) => self.lookup(*local_idx),
            ir::ExprKind::Unop { operand, .. } => {
                // NOTE this is a bit of a hack, but at present the only unary operator only takes
                // u64 types
                if self.lookup(*operand)? != self.ty_sess.mk_u64() {
                    Err(Diagnostic::new_error(
                        "type mismatch",
                        Label::new(
                            self.file_id,
                            span,
                            "argument to unary operator must have type u64",
                        ),
                    ))
                } else {
                    Ok(self.ty_sess.mk_u64())
                }
            }
            ir::ExprKind::Binop { left, right, .. } => {
                // NOTE same hack works here because at present all binary operators only take u64
                // types
                if self.lookup(*left)? != self.ty_sess.mk_u64() {
                    Err(Diagnostic::new_error(
                        "type mismatch",
                        Label::new(
                            self.file_id,
                            left.span(),
                            "arguments to binary operator must have type u64",
                        ),
                    ))
                } else if self.lookup(*right)? != self.ty_sess.mk_u64() {
                    Err(Diagnostic::new_error(
                        "type mismatch",
                        Label::new(
                            self.file_id,
                            right.span(),
                            "arguments to binary operator must have type u64",
                        ),
                    ))
                } else {
                    Ok(self.ty_sess.mk_u64())
                }
            }
            ir::ExprKind::Call { target, args } => {
                let fn_ty = self.global_ctx.lookup(*target, span)?;
                let param_count = self.ty_sess.ty_kind(fn_ty).param_count().ok_or_else(|| {
                    Diagnostic::new_bug(
                        "failed to read fn type",
                        Label::new(
                            self.file_id,
                            span,
                            "could not get parameter count for function",
                        ),
                    )
                })?;
                if args.len() != param_count {
                    return Err(Diagnostic::new_error(
                        "argument count misamtch",
                        Label::new(
                            self.file_id,
                            span,
                            &format!(
                                "function expected {} parameters but {} were found",
                                param_count,
                                args.len()
                            ),
                        ),
                    ));
                }
                for (param_idx, local_idx) in args.iter() {
                    let arg_ty = self.lookup(*local_idx)?;
                    let param_ty =
                        self.ty_sess
                            .ty_kind(fn_ty)
                            .param_ty(param_idx)
                            .ok_or_else(|| {
                                Diagnostic::new_bug(
                                    "failed to read fn type",
                                    Label::new(
                                        self.file_id,
                                        local_idx.span(),
                                        "parameter type could not be read",
                                    ),
                                )
                            })?;
                    if arg_ty != param_ty {
                        return Err(Diagnostic::new_error(
                            "type mismatch",
                            Label::new(
                                self.file_id,
                                local_idx.span(),
                                "argument types and parameter types do not match",
                            ),
                        ));
                    }
                }
                self.ty_sess.ty_kind(fn_ty).return_ty().ok_or_else(|| {
                    Diagnostic::new_bug(
                        "failed to read fn type",
                        Label::new(self.file_id, span, "return type could not be read"),
                    )
                })
            }
            ir::ExprKind::Variant {
                ty,
                discriminant,
                body,
            } => {
                let body_ty = self.lookup(*body)?;
                let variant_ty = self
                    .ty_sess
                    .ty_kind(*ty)
                    .variant_ty(*discriminant)
                    .ok_or_else(|| {
                        Diagnostic::new_bug(
                            "failed to read variant of enum",
                            Label::new(
                                self.file_id,
                                span,
                                &format!(
                                    "the type of {:?} could not be read from the enum",
                                    discriminant
                                ),
                            ),
                        )
                    })?;
                if body_ty != variant_ty {
                    Err(Diagnostic::new_error(
                        "type mismatch",
                        Label::new(
                            self.file_id,
                            body.span(),
                            "enum variant cannot be instantiated using the given body",
                        ),
                    ))
                } else {
                    Ok(*ty)
                }
            }
            ir::ExprKind::Record { ty, fields } => {
                for (field_idx, local_idx) in fields.iter() {
                    let body_ty = self.lookup(*local_idx)?;
                    let field_ty =
                        self.ty_sess
                            .ty_kind(*ty)
                            .field_ty(field_idx)
                            .ok_or_else(|| {
                                Diagnostic::new_bug(
                                    "failed to read field of struct",
                                    Label::new(
                                        self.file_id,
                                        span,
                                        &format!(
                                            "the type of {:?} could not be read from the struct",
                                            field_idx
                                        ),
                                    ),
                                )
                            })?;
                    if body_ty != field_ty {
                        return Err(Diagnostic::new_error(
                            "type mismatch",
                            Label::new(
                                self.file_id,
                                local_idx.span(),
                                "struct field cannot be instantiated using the given body",
                            ),
                        ));
                    }
                }
                Ok(*ty)
            }
        }
    }

    fn check_expr(&mut self, expr: &ir::Expr) -> Result<ty::Ty> {
        self.check_expr_kind(&expr.kind, expr.span)
    }

    fn check_pattern(
        &mut self,
        source_ty: ty::Ty,
        pattern_kind: &ir::PatternKind,
        span: Span,
    ) -> Result<ty::Ty> {
        match pattern_kind {
            ir::PatternKind::Literal(_) => Ok(self.ty_sess.mk_u64()),
            ir::PatternKind::Ident(binding) => {
                self.bind(*binding, source_ty)?;
                Ok(source_ty)
            }
            ir::PatternKind::Variant {
                ty,
                discriminant,
                binding,
            } => {
                let variant_ty = self
                    .ty_sess
                    .ty_kind(*ty)
                    .variant_ty(*discriminant)
                    .ok_or_else(|| {
                        Diagnostic::new_bug(
                            "failed to read variant of enum",
                            Label::new(
                                self.file_id,
                                span,
                                &format!(
                                    "the type of {:?} could not be read from the enum",
                                    discriminant
                                ),
                            ),
                        )
                    })?;
                self.bind(*binding, variant_ty)?;
                Ok(*ty)
            }
            ir::PatternKind::Record { ty, fields } => {
                for (field_idx, binding) in fields.iter() {
                    let field_ty =
                        self.ty_sess
                            .ty_kind(*ty)
                            .field_ty(field_idx)
                            .ok_or_else(|| {
                                Diagnostic::new_bug(
                                    "failed to read field of struct",
                                    Label::new(
                                        self.file_id,
                                        binding.span(),
                                        &format!(
                                            "the type of {:?} could not be read from the struct",
                                            field_idx
                                        ),
                                    ),
                                )
                            })?;
                    self.bind(*binding, field_ty)?;
                }
                Ok(*ty)
            }
        }
    }

    fn check_instruction(&mut self, instruction: &ir::InstructionKind, span: Span) -> Result<()> {
        match instruction {
            ir::InstructionKind::Let { binding, ty, expr } => {
                let bound_ty = self.check_expr(expr)?;
                match ty {
                    Some(ty) if *ty != bound_ty => return Err(Diagnostic::new_error(
                        "type mismatch",
                        Label::new(
                            self.file_id,
                            span,
                            "declared type for let binding does not match type of bound expression",
                        ),
                    )),
                    _ => {
                        self.bind(*binding, bound_ty)?;
                    }
                }
            }
            ir::InstructionKind::Mark(local_idx, ty)
            | ir::InstructionKind::Unmark(local_idx, ty)
            | ir::InstructionKind::Free(local_idx, ty) => {
                // NOTE this would want extending to cover all primitive types given that any
                // further types were added
                if self.lookup(*local_idx)? == self.ty_sess.mk_u64() {
                    return Err(Diagnostic::new_error(
                        "type mismatch",
                        Label::new(
                            self.file_id,
                            local_idx.span(),
                            "cannot free/mark/unmark primitive types",
                        ),
                    ));
                }
                if self.lookup(*local_idx)? != *ty {
                    return Err(Diagnostic::new_bug(
                        "type mismatch",
                        Label::new(
                            self.file_id,
                            local_idx.span(),
                            "attempted to free/mark/unmark data of one type as another",
                        ),
                    ));
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn check_block(&mut self, block: &ir::Block) -> Result<()> {
        for instruction in block.instructions.iter() {
            self.check_instruction(&instruction.kind, instruction.span)?;
        }
        self.check_terminator(&block.terminator)
    }

    fn check_terminator(&mut self, terminator: &ir::Terminator) -> Result<()> {
        match terminator {
            ir::Terminator::Return(local_idx) => {
                let body_ty = self.lookup(*local_idx)?;
                if body_ty != self.prototype.return_ty {
                    Err(Diagnostic::new_error(
                        "type mismatch",
                        Label::new(
                            self.file_id,
                            local_idx.span(),
                            "return type does not match type of returned expression",
                        ),
                    ))
                } else {
                    Ok(())
                }
            }
            ir::Terminator::Match { source, arms } => {
                let source_ty = self.lookup(*source)?;
                for arm in arms.iter() {
                    let pattern_ty = self.check_pattern(source_ty, &arm.pattern, arm.span)?;
                    if pattern_ty != source_ty {
                        return Err(Diagnostic::new_error(
                                "type mismatch",
                                Label::new(
                                    self.file_id,
                                    source.span(),
                                    "match arm contains pattern with type incompatible with that of the match source"
                                )
                            ));
                    }
                    self.check_block(&arm.target)?;
                }
                Ok(())
            }
        }
    }

    fn check_entry(&mut self, entry: &ir::Entry) -> Result<()> {
        for (param_idx, binding) in entry.param_bindings.iter() {
            let param_ty = self
                .prototype
                .params
                .get(param_idx)
                .copied()
                .ok_or_else(|| {
                    Diagnostic::new_bug(
                        "failed to read fn type",
                        Label::new(
                            self.file_id,
                            binding.span(),
                            "parameter type could not be read",
                        ),
                    )
                })?;
            self.bind(*binding, param_ty)?;
        }
        self.check_block(&entry.body)
    }

    fn check_def(
        global_ctx: &'tcx TyCtx<'tcx>,
        def: &ir::Def,
    ) -> Result<IdxVec<ir::LocalIdx, ty::Ty>> {
        let prototype = global_ctx
            .ty_sess
            .ty_kind(def.ty)
            .as_prototype()
            .cloned()
            .ok_or_else(|| {
                Diagnostic::new_bug(
                    "failed to read fn type",
                    Label::new(
                        global_ctx.file_id,
                        def.span,
                        "function prototype could not be read",
                    ),
                )
            })?;
        let mut local_ctx = LocalTyCtx {
            global_ctx,
            prototype,
            tys: HashMap::new(),
        };
        local_ctx.check_entry(&def.entry)?;
        local_ctx
            .tys
            .into_iter()
            .map(|(idx, (_, ty))| (idx, ty))
            .collect::<HashMap<_, _>>()
            .into_idx_vec()
            .ok_or_else(|| {
                Diagnostic::new_bug(
                    "failed to collect type environment for function",
                    Label::new(
                        global_ctx.file_id,
                        def.span,
                        "not all local indices were typed",
                    ),
                )
            })
    }
}

impl<'tcx> Deref for LocalTyCtx<'tcx> {
    type Target = TyCtx<'tcx>;

    fn deref(&self) -> &Self::Target {
        self.global_ctx
    }
}

pub fn ty_check(sess: &Opts, file_id: FileId, ty_sess: &ty::TySess, ir: &ir::Ir) -> Result<TyEnv> {
    TyCtx::check_ir(sess, file_id, ty_sess, ir)
}
