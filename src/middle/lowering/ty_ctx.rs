use crate::common::Indexable;
use crate::diagnostics::{Diagnostic, FileId, Label, Result, Span};
use crate::driver::Opts;
use crate::syntax::ast;
use crate::ty;

use std::collections::HashMap;

#[derive(Debug)]
pub(super) struct TyLoweringCtx<'ast> {
    sess: &'ast Opts,
    file_id: FileId,
    ty_sess: ty::TySess,
    u64_ty: ty::Ty,
    tys: HashMap<&'ast ast::Ident, ty::Ty>,
    variants: HashMap<ty::Ty, HashMap<&'ast ast::Ident, ty::VariantIdx>>,
    fields: HashMap<ty::Ty, HashMap<&'ast ast::Ident, ty::FieldIdx>>,
}

impl<'ast> TyLoweringCtx<'ast> {
    pub(super) fn new(sess: &'ast Opts, file_id: FileId) -> TyLoweringCtx<'ast> {
        let ty_sess = ty::TySess::new();
        let u64_ty = ty_sess.mk_u64();
        TyLoweringCtx {
            sess,
            file_id,
            ty_sess,
            u64_ty,
            tys: HashMap::new(),
            variants: HashMap::new(),
            fields: HashMap::new(),
        }
    }

    pub(super) fn bind(&mut self, ident: &'ast ast::Ident, span: Span, ty: ty::Ty) -> Result<()> {
        if self.tys.get(ident).is_some() {
            return Err(Diagnostic::new_error(
                "previously bound type name",
                Label::new(
                    self.file_id,
                    span,
                    &format!("attempt to rebind '{}' here", ident),
                ),
            ));
        }
        debug!("bound '{}' as {:?}", ident, ty);
        self.tys.insert(ident, ty);
        Ok(())
    }

    pub(super) fn lookup(&self, ident: &'ast ast::Ident, span: Span) -> Result<ty::Ty> {
        if let Some(ty) = self.tys.get(ident) {
            return Ok(*ty);
        }
        Err(Diagnostic::new_error(
            "reference to unbound type name",
            Label::new(
                self.file_id,
                span,
                &format!("'{}' is not bound as a type", ident),
            ),
        ))
    }

    #[inline]
    pub(super) fn lookup_ty(&self, ty: &'ast ast::Ty, span: Span) -> Result<ty::Ty> {
        match ty {
            ast::Ty::U64 => Ok(self.u64_ty),
            ast::Ty::TyName(ident) => self.lookup(ident, span),
        }
    }

    #[inline]
    pub(super) fn lookup_binding(&self, binding: &'ast ast::Binding) -> Result<ty::Ty> {
        self.lookup_ty(&binding.ty, binding.ty.span())
    }

    pub(super) fn lookup_variant(
        &self,
        ty: ty::Ty,
        variant_name: &'ast ast::Ident,
        span: Span,
    ) -> Result<ty::VariantIdx> {
        if let Some(variants) = self.variants.get(&ty) {
            if let Some(variant) = variants.get(variant_name) {
                Ok(*variant)
            } else {
                Err(Diagnostic::new_error(
                    "type usage error",
                    Label::new(
                        self.file_id,
                        span,
                        &format!("'{}' is not a variant of the type given", variant_name),
                    ),
                ))
            }
        } else {
            Err(Diagnostic::new_error(
                "type usage error",
                Label::new(
                    self.file_id,
                    span,
                    "can't reference variants of a type that is not an enum",
                ),
            ))
        }
    }

    pub(super) fn lookup_field(
        &self,
        ty: ty::Ty,
        field_name: &'ast ast::Ident,
        span: Span,
    ) -> Result<ty::FieldIdx> {
        if let Some(fields) = self.fields.get(&ty) {
            if let Some(field) = fields.get(field_name) {
                Ok(*field)
            } else {
                Err(Diagnostic::new_error(
                    "type usage error",
                    Label::new(
                        self.file_id,
                        span,
                        &format!("'{}' is not a field of the type given", field_name),
                    ),
                ))
            }
        } else {
            Err(Diagnostic::new_error(
                "type usage error",
                Label::new(
                    self.file_id,
                    span,
                    "can't reference fields of a type that is not a struct",
                ),
            ))
        }
    }

    pub(super) fn register<T>(&mut self, items: T) -> Result<()>
    where
        T: Iterator<Item = &'ast ast::Item>,
    {
        for item in items {
            match item {
                ast::Item::Enum(def) => {
                    let ty = self.ty_sess.mk_enum();
                    self.bind(&def.name, def.name.span(), ty)?;
                }
                ast::Item::Struct(def) => {
                    let ty = self.ty_sess.mk_struct();
                    self.bind(&def.name, def.name.span(), ty)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    pub(super) fn lower<T>(&mut self, items: T) -> Result<()>
    where
        T: Iterator<Item = &'ast ast::Item>,
    {
        let file_id = self.file_id;
        for item in items {
            match item {
                ast::Item::Enum(def) => {
                    let mut variant_tys = HashMap::new();
                    for binding in def.variants.iter() {
                        if variant_tys
                            .insert(&*binding.binder, self.lookup_binding(binding)?)
                            .is_some()
                        {
                            return Err(Diagnostic::new_error(
                                "attempted to rebind enum variant",
                                Label::new(
                                    file_id,
                                    binding.span(),
                                    "a variant with this name already exists",
                                ),
                            ));
                        }
                    }
                    let (variants, index) = variant_tys.reindex::<ty::VariantIdx>();
                    let ty = self.lookup(&def.name, def.name.span())?;
                    self.variants.insert(ty, variants);
                    self.ty_sess
                        .ty_kind_mut(ty)
                        .as_enum_mut()
                        .map(|desc| {
                            desc.variants = index;
                        })
                        .ok_or_else(|| {
                            Diagnostic::new_bug(
                                "attempted to set variants for non-enum type",
                                Label::new(
                                    file_id,
                                    def.name.span(),
                                    "lowered type for this definition is not an enum",
                                ),
                            )
                        })?;
                }
                ast::Item::Struct(def) => {
                    let mut field_tys = HashMap::new();
                    for binding in def.fields.iter() {
                        if field_tys
                            .insert(&*binding.binder, self.lookup_binding(binding)?)
                            .is_some()
                        {
                            return Err(Diagnostic::new_error(
                                "attempted to rebind field type",
                                Label::new(
                                    file_id,
                                    binding.span(),
                                    "a field with this name already exists",
                                ),
                            ));
                        }
                    }
                    let (fields, index) = field_tys.reindex::<ty::FieldIdx>();
                    let ty = self.lookup(&def.name, def.name.span())?;
                    self.fields.insert(ty, fields);
                    self.ty_sess
                        .ty_kind_mut(ty)
                        .as_struct_mut()
                        .map(|desc| {
                            desc.fields = index;
                        })
                        .ok_or_else(|| {
                            Diagnostic::new_bug(
                                "attempted to set fields for non-struct type",
                                Label::new(
                                    file_id,
                                    def.name.span(),
                                    "lowered type for this definition is not a struct",
                                ),
                            )
                        })?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    pub(super) fn ty_sess(&self) -> &ty::TySess {
        &self.ty_sess
    }

    pub(super) fn into_ty_sess(self) -> ty::TySess {
        self.ty_sess
    }
}
