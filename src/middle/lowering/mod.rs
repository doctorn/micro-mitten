use crate::common::{Idx, IdxVec, Idxr};
use crate::diagnostics::{Diagnostic, FileId, Label, Result, Span, Spanned};
use crate::driver::Opts;
use crate::middle::ir;
use crate::syntax::ast;
use crate::{pp, ty};

use std::collections::HashMap;
use std::ops::Deref;

use self::ctx::LoweringCtx;
use self::ty_ctx::TyLoweringCtx;

mod ctx;
mod ty_ctx;

#[derive(Debug)]
struct LoweringSess<'ast> {
    sess: &'ast Opts,
    file_id: FileId,
    tys: TyLoweringCtx<'ast>,
    ir: ir::Ir,
    bind_points: IdxVec<ir::DefIdx, Span>,
    global_map: HashMap<&'ast ast::Ident, ir::DefIdx>,
}

impl<'ast> LoweringSess<'ast> {
    fn new(sess: &'ast Opts, file_id: FileId, tys: TyLoweringCtx<'ast>) -> LoweringSess<'ast> {
        LoweringSess {
            sess,
            file_id,
            tys,
            ir: ir::Ir {
                defs: IdxVec::new(),
            },
            bind_points: IdxVec::new(),
            global_map: HashMap::new(),
        }
    }

    fn lookup(&self, ident: &'ast ast::Ident, span: Span) -> Result<ir::DefIdx> {
        if let Some(def_idx) = self.global_map.get(ident) {
            Ok(*def_idx)
        } else {
            Err(Diagnostic::new_error(
                "reference to unbound function symbol",
                Label::new(
                    self.file_id,
                    span,
                    &format!("'{}' is not bound as a function symbol", ident),
                ),
            ))
        }
    }

    fn bind(&mut self, ident: &'ast ast::Ident, span: Span) -> Result<ir::DefIdx> {
        if let Some(def_idx) = self.global_map.get(ident) {
            Err(Diagnostic::new_error(
                "attempt to rebind function name",
                Label::new(
                    self.file_id,
                    span,
                    &format!("'{}' is already bound to a function", ident),
                ),
            )
            .with_secondary_labels(vec![Label::new(
                self.file_id,
                self.bind_points[*def_idx],
                "previously bound here",
            )]))
        } else {
            let def_idx = self.bind_points.push(span);
            self.global_map.insert(ident, def_idx);
            Ok(def_idx)
        }
    }

    fn lower_decl(&mut self, decl: &'ast ast::FnDecl) -> Result<ir::Def> {
        if super::RESERVED_NAMES.contains(&&**decl.name) {
            return Err(Diagnostic::new_error(
                "use of reserved name",
                Label::new(
                    self.file_id,
                    decl.name.span(),
                    &format!("'{}' is reserved for use by the compiler", &**decl.name),
                ),
            ));
        }
        let def_idx = self.lookup(&decl.name, decl.name.span())?;
        let local_idxr = Idxr::new();
        let block_idxr = Idxr::new();
        let mut lcx = LoweringCtx::new(self, &local_idxr, &block_idxr, def_idx);
        let mut param_tys = IdxVec::new();
        let mut param_bindings: IdxVec<ty::ParamIdx, ir::LocalIdx> = IdxVec::new();
        for binding in decl.params.iter() {
            let local_idx = local_idxr.next().with_span(binding.span());
            if lcx.bind(&binding.binder, local_idx).is_some() {
                return Err(Diagnostic::new_error(
                    "attempted to rebind formal parameter",
                    Label::new(
                        self.file_id,
                        binding.span(),
                        "a formal parameter with this name already exists",
                    ),
                ));
            }
            let param_idx = param_bindings.push(local_idx.with_span(binding.span()));
            debug_assert_eq!(param_idx.index(), local_idx.index());
            param_tys.push(self.tys.lookup_ty(&binding.ty, binding.ty.span())?);
        }
        let return_ty = self.tys.lookup_ty(&decl.return_ty, decl.return_ty.span())?;
        Ok(ir::Def {
            def_idx,
            name: decl.name.clone(),
            span: decl.name.span(),
            ty: self.tys.ty_sess().mk_fn(return_ty, param_tys),
            entry: lcx.lower_entry(param_bindings, &decl.body, decl.body.span())?,
            local_idxr,
        })
    }

    fn register<T>(&mut self, items: T) -> Result<()>
    where
        T: Iterator<Item = &'ast ast::Item>,
    {
        for item in items {
            if let ast::Item::Fn(def) = item {
                self.bind(&def.name, def.name.span())?;
            }
        }
        Ok(())
    }

    fn lower<T>(&mut self, items: T) -> Result<()>
    where
        T: Iterator<Item = &'ast ast::Item>,
    {
        for item in items {
            if let ast::Item::Fn(decl) = item {
                let def = self.lower_decl(decl)?;
                self.ir.defs.push(def);
            }
        }
        Ok(())
    }

    fn complete(self) -> (ir::Ir, ty::TySess) {
        if self.sess.dump_ir {
            pp!(&self.ir);
        }
        (self.ir, self.tys.into_ty_sess())
    }
}

pub fn lower<'ast>(
    sess: &Opts,
    file_id: FileId,
    ast: &'ast ast::Ast,
) -> Result<(ir::Ir, ty::TySess)> {
    let mut tys = TyLoweringCtx::new(sess, file_id);
    tys.register(ast.items.iter().map(Spanned::deref))?;
    tys.lower(ast.items.iter().map(Spanned::deref))?;
    let mut lowering_sess = LoweringSess::new(sess, file_id, tys);
    lowering_sess.register(ast.items.iter().map(Spanned::deref))?;
    lowering_sess.lower(ast.items.iter().map(Spanned::deref))?;
    Ok(lowering_sess.complete())
}
