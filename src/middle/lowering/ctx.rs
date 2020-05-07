use crate::common::{IdxVec, Idxr, IntoIdxVec};
use crate::diagnostics::{Diagnostic, Label, Result, Span};
use crate::middle::ir;
use crate::syntax::ast;
use crate::ty;

use std::collections::HashMap;

use super::LoweringSess;

#[derive(Debug)]
pub(super) struct LoweringCtx<'lcx, 'ast> {
    sess: &'lcx LoweringSess<'ast>,
    local_idxr: &'lcx Idxr<ir::LocalIdx>,
    block_idxr: &'lcx Idxr<ir::BlockIdx>,
    def_idx: ir::DefIdx,
    parent: Option<&'lcx LoweringCtx<'lcx, 'ast>>,
    local_map: HashMap<&'ast ast::Ident, ir::LocalIdx>,
    instructions: Vec<ir::Instruction>,
}

impl<'lcx, 'ast> LoweringCtx<'lcx, 'ast> {
    pub(super) fn new(
        sess: &'lcx LoweringSess<'ast>,
        local_idxr: &'lcx Idxr<ir::LocalIdx>,
        block_idxr: &'lcx Idxr<ir::BlockIdx>,
        def_idx: ir::DefIdx,
    ) -> LoweringCtx<'lcx, 'ast> {
        LoweringCtx {
            sess,
            local_idxr,
            block_idxr,
            def_idx,
            parent: None,
            local_map: HashMap::new(),
            instructions: vec![],
        }
    }

    fn mk_child(&'lcx self) -> LoweringCtx<'lcx, 'ast> {
        LoweringCtx {
            sess: self.sess,
            local_idxr: self.local_idxr,
            block_idxr: self.block_idxr,
            def_idx: self.def_idx,
            parent: Some(self),
            local_map: HashMap::new(),
            instructions: vec![],
        }
    }

    fn lookup(&self, ident: &'ast ast::Ident, span: Span) -> Result<ir::LocalIdx> {
        if let Some(local_idx) = self.local_map.get(ident) {
            Ok(local_idx.with_span(span))
        } else if let Some(parent) = self.parent {
            parent.lookup(ident, span)
        } else {
            Err(Diagnostic::new_error(
                "reference to unbound variable",
                Label::new(
                    self.sess.file_id,
                    span,
                    &format!("'{}' is not bound here (while lowering)", ident),
                ),
            ))
        }
    }

    #[inline]
    pub(super) fn bind(
        &mut self,
        ident: &'ast ast::Ident,
        local_idx: ir::LocalIdx,
    ) -> Option<ir::LocalIdx> {
        self.local_map.insert(ident, local_idx)
    }

    #[inline]
    fn lower_unop_kind(&self, kind: ast::UnopKind) -> ir::UnopKind {
        match kind {
            ast::UnopKind::Not => ir::UnopKind::Not,
        }
    }

    #[inline]
    fn lower_binop_kind(&self, kind: ast::BinopKind) -> ir::BinopKind {
        match kind {
            ast::BinopKind::Plus => ir::BinopKind::Plus,
            ast::BinopKind::Minus => ir::BinopKind::Minus,
            ast::BinopKind::Mul => ir::BinopKind::Mul,
            ast::BinopKind::Div => ir::BinopKind::Div,
            ast::BinopKind::Less => ir::BinopKind::Less,
            ast::BinopKind::Leq => ir::BinopKind::Leq,
            ast::BinopKind::Greater => ir::BinopKind::Greater,
            ast::BinopKind::Geq => ir::BinopKind::Geq,
            ast::BinopKind::Eq => ir::BinopKind::Eq,
            ast::BinopKind::Neq => ir::BinopKind::Neq,
            ast::BinopKind::And => ir::BinopKind::And,
            ast::BinopKind::Or => ir::BinopKind::Or,
            ast::BinopKind::Xor => ir::BinopKind::Xor,
            ast::BinopKind::LShift => ir::BinopKind::LShift,
            ast::BinopKind::RShift => ir::BinopKind::RShift,
        }
    }

    fn lower_expr_kind(&mut self, expr: &'ast ast::Expr, span: Span) -> Result<ir::ExprKind> {
        Ok(match expr {
            ast::Expr::Literal(literal) => ir::ExprKind::Literal(*literal),
            ast::Expr::Var(ident) => ir::ExprKind::Var(self.lookup(ident, span)?),
            ast::Expr::Unop { kind, operand } => ir::ExprKind::Unop {
                kind: self.lower_unop_kind(**kind),
                operand: self.lower_expr(None, operand, operand.span())?,
            },
            ast::Expr::Binop { kind, left, right } => ir::ExprKind::Binop {
                kind: self.lower_binop_kind(**kind),
                left: self.lower_expr(None, left, left.span())?,
                right: self.lower_expr(None, right, right.span())?,
            },
            ast::Expr::Call { target, args } => {
                let mut lowered_args = IdxVec::new();
                for arg in args.iter() {
                    lowered_args.push(self.lower_expr(None, arg, arg.span())?);
                }
                ir::ExprKind::Call {
                    target: self.sess.lookup(target, target.span())?,
                    args: lowered_args,
                }
            }
            ast::Expr::Variant {
                enum_name,
                discriminant,
                body,
            } => {
                let enum_ty = self.sess.tys.lookup(&enum_name, enum_name.span())?;
                ir::ExprKind::Variant {
                    ty: enum_ty,
                    discriminant: self.sess.tys.lookup_variant(
                        enum_ty,
                        &discriminant,
                        discriminant.span(),
                    )?,
                    body: self.lower_expr(None, body, body.span())?,
                }
            }
            ast::Expr::Record {
                struct_name,
                fields,
            } => {
                let struct_ty = self.sess.tys.lookup(&struct_name, struct_name.span())?;
                let mut field_bindings = HashMap::new();
                for (field, body) in fields.iter() {
                    let lowered = self.lower_expr(None, body, body.span())?;
                    if let Some(idx) = field_bindings.insert(
                        self.sess.tys.lookup_field(struct_ty, field, field.span())?,
                        lowered,
                    ) {
                        return Err(Diagnostic::new_error(
                            "malformed struct initialiser",
                            Label::new(
                                self.sess.file_id,
                                span,
                                "attempted to initialise the same field twice",
                            ),
                        )
                        .with_secondary_labels(vec![
                            Label::new(
                                self.sess.file_id,
                                lowered.span(),
                                &format!("attempted to initialise '{}' here", &**field),
                            ),
                            Label::new(
                                self.sess.file_id,
                                idx.span(),
                                "but it was already initialised here",
                            ),
                        ]));
                    }
                }
                if let Some(fields) = field_bindings.into_idx_vec() {
                    ir::ExprKind::Record {
                        ty: struct_ty,
                        fields,
                    }
                } else {
                    return Err(Diagnostic::new_error(
                        "malformed struct initialiser",
                        Label::new(self.sess.file_id, span, "not all fileds initialised"),
                    ));
                }
            }
        })
    }

    fn lower_expr(
        &mut self,
        ty: Option<ty::Ty>,
        expr: &'ast ast::Expr,
        span: Span,
    ) -> Result<ir::LocalIdx> {
        let kind = self.lower_expr_kind(expr, span)?;
        Ok(match kind {
            ir::ExprKind::Var(idx) => idx,
            _ => {
                let idx = self.local_idxr.next();
                self.instructions.push(ir::Instruction {
                    span,
                    kind: ir::InstructionKind::Let {
                        binding: idx,
                        ty,
                        expr: ir::Expr {
                            local_idx: idx,
                            span,
                            kind,
                        },
                    },
                });
                idx
            }
        }
        .with_span(span))
    }

    fn lower_arm(
        &mut self,
        pattern: &'ast ast::Pattern,
        body: &'ast ast::Term,
        pattern_span: Span,
        body_span: Span,
    ) -> Result<ir::Arm> {
        let mut ctx = self.mk_child();
        let pattern = match pattern {
            ast::Pattern::Literal(literal) => ir::PatternKind::Literal(*literal),
            ast::Pattern::Ident(ident) => {
                let local_idx = self.local_idxr.next().with_span(pattern_span);
                ctx.bind(ident, local_idx);
                ir::PatternKind::Ident(local_idx)
            }
            ast::Pattern::Variant {
                enum_name,
                discriminant,
                bound,
            } => {
                let local_idx = self.local_idxr.next().with_span(bound.span());
                ctx.bind(&bound, local_idx);
                let ty = self.sess.tys.lookup(&enum_name, enum_name.span())?;
                let discriminant =
                    self.sess
                        .tys
                        .lookup_variant(ty, &discriminant, discriminant.span())?;
                ir::PatternKind::Variant {
                    ty,
                    discriminant,
                    binding: local_idx,
                }
            }
            ast::Pattern::Record {
                struct_name,
                fields,
            } => {
                let ty = self.sess.tys.lookup(&struct_name, struct_name.span())?;
                let mut field_bindings = HashMap::new();
                for (field, bound) in fields {
                    let local_idx = self.local_idxr.next().with_span(bound.span());
                    ctx.bind(&bound, local_idx);
                    let field = self.sess.tys.lookup_field(ty, &field, field.span())?;
                    field_bindings.insert(field, local_idx);
                }
                if let Some(fields) = field_bindings.into_idx_vec() {
                    ir::PatternKind::Record { ty, fields }
                } else {
                    return Err(Diagnostic::new_error(
                        "malformed match arm",
                        Label::new(
                            self.sess.file_id,
                            pattern_span,
                            "not all fields are matched",
                        ),
                    ));
                }
            }
        };
        Ok(ir::Arm {
            span: pattern_span.merge(body_span),
            pattern,
            target: Box::new(ctx.lower_term_to_block(body, body_span)?),
        })
    }

    fn lower_term(&mut self, term: &'ast ast::Term, span: Span) -> Result<ir::Terminator> {
        match term {
            ast::Term::Let {
                binder,
                annotation,
                expr,
                body,
            } => {
                let ty = match annotation {
                    Some(ty) => Some(self.sess.tys.lookup_ty(ty, ty.span())?),
                    _ => None,
                };
                let idx = self.lower_expr(ty, expr, expr.span())?;
                self.bind(&binder, idx);
                self.lower_term(&body, body.span())
            }
            ast::Term::Match { source, arms } => {
                let source = self.lower_expr(None, source, source.span())?;
                let mut lowered_arms = vec![];
                for (pattern, body) in arms.iter() {
                    lowered_arms.push(self.lower_arm(
                        &pattern,
                        &body,
                        pattern.span(),
                        body.span(),
                    )?);
                }
                Ok(ir::Terminator::Match {
                    source,
                    arms: lowered_arms,
                })
            }
            ast::Term::If {
                source,
                then,
                otherwise,
            } => {
                let source = self.lower_expr(None, source, source.span())?;
                Ok(ir::Terminator::Match {
                    source,
                    arms: vec![
                        ir::Arm {
                            span: otherwise.span(),
                            pattern: ir::PatternKind::Literal(0),
                            target: Box::new(
                                self.mk_child()
                                    .lower_term_to_block(&otherwise, otherwise.span())?,
                            ),
                        },
                        ir::Arm {
                            span: then.span(),
                            pattern: ir::PatternKind::Ident(
                                self.local_idxr.next().with_span(source.span()),
                            ),
                            target: Box::new(
                                self.mk_child().lower_term_to_block(&then, then.span())?,
                            ),
                        },
                    ],
                })
            }
            ast::Term::Return(expr) => {
                Ok(ir::Terminator::Return(self.lower_expr(None, expr, span)?))
            }
        }
    }

    fn lower_term_to_block(mut self, term: &'ast ast::Term, span: Span) -> Result<ir::Block> {
        let block_idx = self.block_idxr.next();
        let terminator = self.lower_term(term, span)?;
        Ok(ir::Block {
            owner: self.def_idx,
            block_idx,
            span,
            instructions: self.instructions,
            terminator,
        })
    }

    pub(super) fn lower_entry(
        self,
        param_bindings: IdxVec<ty::ParamIdx, ir::LocalIdx>,
        term: &'ast ast::Term,
        span: Span,
    ) -> Result<ir::Entry> {
        Ok(ir::Entry {
            owner: self.def_idx,
            param_bindings,
            body: self.lower_term_to_block(term, span)?,
        })
    }
}
