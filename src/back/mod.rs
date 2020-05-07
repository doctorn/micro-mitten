use crate::common::Idx;
use crate::diagnostics::{Diagnostic, FileId, Label, Result, Span};
use crate::driver::{GcStrategy, Opts};
use crate::middle::ir;
use crate::ty;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::AddressSpace;

use std::path::Path;

use self::ctx::GenCtx;

mod ctx;

const GC_MALLOC: &str = "GC_malloc";

const MITTEN_FREE: &str = "mitten_free";
const MITTEN_RESET: &str = "mitten_reset";

struct GenSess<'gen, 'ctx> {
    sess: &'gen Opts,
    file_id: FileId,
    context: &'ctx Context,
    builder: &'gen Builder<'ctx>,
    module: &'gen Module<'ctx>,
    ty_sess: &'gen ty::TySess,
    ir: &'gen ir::Ir,
}

impl<'gen, 'ctx> GenSess<'gen, 'ctx> {
    fn bind_def(&self, def: &ir::Def) -> FunctionValue<'ctx> {
        let fn_ty = self.compile_ty(def.ty).into_function_type();
        self.module.add_function(&def.name, fn_ty, None)
    }

    fn bind_library_fns(&self) {
        if self.sess.gc_strategy == GcStrategy::BDW {
            self.module.add_function(
                GC_MALLOC,
                self.context
                    .i8_type()
                    .ptr_type(AddressSpace::Generic)
                    .fn_type(&[self.context.i64_type().as_basic_type_enum()], false),
                None,
            );
        } else if self.sess.gc_strategy == GcStrategy::Proust {
            self.module.add_function(
                MITTEN_RESET,
                self.context.void_type().fn_type(&[], false),
                None,
            );
            self.module.add_function(
                MITTEN_FREE,
                self.context.void_type().fn_type(
                    &[self
                        .context
                        .i8_type()
                        .ptr_type(AddressSpace::Generic)
                        .as_basic_type_enum()],
                    false,
                ),
                None,
            );
        }
    }

    fn lookup_def(&self, def: ir::DefIdx, span: Span) -> Result<FunctionValue<'ctx>> {
        let name = &self.ir.defs.get(def).unwrap().name;
        self.module.get_function(name).ok_or_else(|| {
            Diagnostic::new_bug(
                "attempt to reference unregistered def",
                Label::new(
                    self.file_id,
                    span,
                    &format!("{} is not listed in the LLVM module", name),
                ),
            )
        })
    }

    fn build_reset(&self) {
        self.builder.build_call(
            self.module.get_function(MITTEN_RESET).unwrap(),
            &[],
            "reset",
        );
    }

    fn build_free(&self, ptr: PointerValue<'ctx>) {
        let ptr = self.builder.build_pointer_cast(
            ptr,
            self.context.i8_type().ptr_type(AddressSpace::Generic),
            "raw",
        );
        self.builder.build_call(
            self.module.get_function(MITTEN_FREE).unwrap(),
            &[ptr.as_basic_value_enum()],
            "free",
        );
    }

    fn build_alloc(&self, ty: ty::Ty, name: &str, span: Span) -> Result<PointerValue<'ctx>> {
        let ty = self.compile_basic_ty_unboxed(ty);
        match self.sess.gc_strategy {
            GcStrategy::BDW => {
                let size = ty
                    .size_of()
                    .ok_or_else(|| {
                        Diagnostic::new_bug(
                            "failed to build alloc call",
                            Label::new(self.file_id, span, "attempted to allocate unsized type"),
                        )
                    })?
                    .as_basic_value_enum();
                let alloc = self.module.get_function(GC_MALLOC).unwrap();
                let mem = self
                    .builder
                    .build_call(alloc, &[size], "mem")
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| {
                        Diagnostic::new_bug(
                            "failed to build GC_malloc call",
                            Label::new(self.file_id, span, "call returned non-basic value"),
                        )
                    })?
                    .into_pointer_value();
                Ok(self
                    .builder
                    .build_pointer_cast(mem, ty.ptr_type(AddressSpace::Generic), name))
            }
            _ => self.builder.build_malloc(ty, name).map_err(|err| {
                Diagnostic::new_bug(
                    "failed to build malloc call",
                    Label::new(self.file_id, span, err),
                )
            }),
        }
    }

    #[inline]
    fn compile_literal(&self, literal: u64) -> IntValue<'ctx> {
        self.context.i64_type().const_int(literal, false)
    }

    #[inline]
    fn compile_variant_idx(&self, idx: ty::VariantIdx) -> IntValue<'ctx> {
        self.compile_literal(idx.index() as u64)
    }

    #[inline]
    fn read_param(&self, function: FunctionValue<'ctx>, idx: ty::ParamIdx) -> BasicValueEnum<'ctx> {
        if let Some(param) = function.get_nth_param(idx.index() as u32) {
            param
        } else {
            panic!(
                "attempted to param with index {} when it does not exist",
                idx.index()
            )
        }
    }

    #[inline]
    unsafe fn gep(&self, ptr: PointerValue<'ctx>, idx: u64, name: &str) -> PointerValue<'ctx> {
        self.builder.build_gep(
            ptr,
            &[
                self.context.i32_type().const_int(0, false),
                self.context.i32_type().const_int(idx, false),
            ],
            name,
        )
    }

    fn mark_ptr(&self, ptr: PointerValue<'ctx>, ty: ty::Ty) -> PointerValue<'ctx> {
        if self.ty_sess.ty_kind(ty).is_enum() {
            unsafe { self.gep(ptr, 2, "mark_ptr") }
        } else if self.ty_sess.ty_kind(ty).is_struct() {
            unsafe {
                self.gep(
                    ptr,
                    self.ty_sess.ty_kind(ty).field_count().unwrap() as u64,
                    "mark_ptr",
                )
            }
        } else {
            panic!("atttempted to read mark of a type that wasn't a struct or an enum")
        }
    }

    fn read_mark(&self, ptr: PointerValue<'ctx>, ty: ty::Ty) -> BasicValueEnum<'ctx> {
        self.builder.build_load(self.mark_ptr(ptr, ty), "mark")
    }

    fn write_mark(&self, ptr: PointerValue<'ctx>, ty: ty::Ty, mark: bool) {
        self.builder.build_store(
            self.mark_ptr(ptr, ty),
            self.context
                .custom_width_int_type(1)
                .const_int(if mark { 1 } else { 0 }, false),
        );
    }

    fn enum_discriminant_ptr(&self, ptr: PointerValue<'ctx>, ty: ty::Ty) -> PointerValue<'ctx> {
        if self.ty_sess.ty_kind(ty).is_enum() {
            unsafe { self.gep(ptr, 0, "discriminant_ptr") }
        } else {
            panic!("attempted to read discriminant of type that is not an enum")
        }
    }

    fn enum_body_ptr(&self, ptr: PointerValue<'ctx>, ty: ty::Ty) -> PointerValue<'ctx> {
        if self.ty_sess.ty_kind(ty).is_enum() {
            unsafe { self.gep(ptr, 1, "body_ptr") }
        } else {
            panic!("attempted to read body of type that is not an enum")
        }
    }

    #[inline]
    fn read_enum_discriminant(
        &self,
        ptr: PointerValue<'ctx>,
        ty: ty::Ty,
    ) -> Result<BasicValueEnum<'ctx>> {
        Ok(self
            .builder
            .build_load(self.enum_discriminant_ptr(ptr, ty), "discriminant"))
    }

    fn read_enum_body(
        &self,
        ptr: PointerValue<'ctx>,
        ty: ty::Ty,
        idx: ty::VariantIdx,
    ) -> BasicValueEnum<'ctx> {
        let variant_ty = self.ty_sess.ty_kind(ty).variant_ty(idx).unwrap();
        let body_ptr = self.enum_body_ptr(ptr, ty);
        if variant_ty != self.ty_sess.mk_u64() {
            let uncast_body = self.builder.build_load(body_ptr, "uncast_body");
            self.builder
                .build_int_to_ptr(
                    uncast_body.into_int_value(),
                    self.compile_basic_ty(variant_ty).into_pointer_type(),
                    "body",
                )
                .into()
        } else {
            self.builder.build_load(body_ptr, "body")
        }
    }

    #[inline]
    fn write_enum_discriminant(&self, ptr: PointerValue<'ctx>, ty: ty::Ty, idx: ty::VariantIdx) {
        self.builder.build_store(
            self.enum_discriminant_ptr(ptr, ty),
            self.context.i64_type().const_int(idx.index() as u64, false),
        );
    }

    fn write_enum_body(
        &self,
        ptr: PointerValue<'ctx>,
        ty: ty::Ty,
        idx: ty::VariantIdx,
        val: BasicValueEnum<'ctx>,
    ) {
        let variant_ty = self.ty_sess.ty_kind(ty).variant_ty(idx).unwrap();
        let body_ptr = self.enum_body_ptr(ptr, ty);
        let cast_body = if variant_ty != self.ty_sess.mk_u64() {
            self.builder
                .build_ptr_to_int(
                    val.into_pointer_value(),
                    self.context.i64_type(),
                    "cast_body",
                )
                .into()
        } else {
            val
        };
        self.builder.build_store(body_ptr, cast_body);
    }

    fn struct_field_ptr(
        &self,
        ptr: PointerValue<'ctx>,
        ty: ty::Ty,
        idx: ty::FieldIdx,
    ) -> PointerValue<'ctx> {
        if self.ty_sess.ty_kind(ty).field_ty(idx).is_some() {
            unsafe {
                self.gep(
                    ptr,
                    idx.index() as u64,
                    &format!("field_{}_ptr", idx.index()),
                )
            }
        } else {
            panic!(
                "attempted to access field with index {} when it does not exist",
                idx.index()
            )
        }
    }

    #[inline]
    fn write_struct_field(
        &self,
        ptr: PointerValue<'ctx>,
        ty: ty::Ty,
        idx: ty::FieldIdx,
        val: BasicValueEnum<'ctx>,
    ) {
        self.builder
            .build_store(self.struct_field_ptr(ptr, ty, idx), val);
    }

    #[inline]
    fn read_struct_field(
        &self,
        ptr: PointerValue<'ctx>,
        ty: ty::Ty,
        idx: ty::FieldIdx,
    ) -> BasicValueEnum<'ctx> {
        self.builder.build_load(
            self.struct_field_ptr(ptr, ty, idx),
            &format!("field_{}", idx.index()),
        )
    }

    fn compile_basic_ty_unboxed(&self, ty: ty::Ty) -> BasicTypeEnum<'ctx> {
        match &*self.ty_sess.ty_kind(ty) {
            ty::TyKind::U64 => self.context.i64_type().into(),
            ty::TyKind::Enum(_) => {
                let mut field_tys = vec![
                    self.context.i64_type().into(),
                    self.context.i64_type().into(),
                ];
                if self.sess.gc_strategy == GcStrategy::Proust {
                    field_tys.push(self.context.custom_width_int_type(1).into());
                }
                self.context.struct_type(field_tys.as_slice(), false).into()
            }
            ty::TyKind::Struct(ty::Struct { fields }) => {
                let mut field_tys = fields
                    .values()
                    .map(|ty| self.compile_basic_ty(*ty))
                    .collect::<Vec<_>>();
                if self.sess.gc_strategy == GcStrategy::Proust {
                    field_tys.push(self.context.custom_width_int_type(1).into());
                }
                self.context.struct_type(field_tys.as_slice(), false).into()
            }
            _ => panic!("attempted to compile function type as basic type"),
        }
    }

    fn compile_basic_ty(&self, ty: ty::Ty) -> BasicTypeEnum<'ctx> {
        let compiled_ty_unboxed = self.compile_basic_ty_unboxed(ty);
        if self.ty_sess.ty_kind(ty).is_u64() {
            compiled_ty_unboxed
        } else {
            compiled_ty_unboxed.ptr_type(AddressSpace::Generic).into()
        }
    }

    fn compile_fn_ty(&self, prototype: &ty::Prototype) -> FunctionType<'ctx> {
        let mut param_tys = Vec::with_capacity(prototype.params.len());
        for ty in prototype.params.values() {
            let compiled_ty = self.compile_basic_ty(*ty);
            param_tys.push(compiled_ty);
        }
        self.compile_basic_ty(prototype.return_ty)
            .fn_type(param_tys.as_slice(), false)
    }

    fn compile_ty(&self, ty: ty::Ty) -> AnyTypeEnum<'ctx> {
        match &*self.ty_sess.ty_kind(ty) {
            ty::TyKind::Fn(prototype) => self.compile_fn_ty(prototype).into(),
            _ => self.compile_basic_ty(ty).as_any_type_enum(),
        }
    }

    fn compile_def(&self, def: &ir::Def) -> Result<FunctionValue<'ctx>> {
        GenCtx::compile_def(self, def)
    }

    fn write_to_ll_file<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        self.module.print_to_file(path).map_err(|e| {
            Diagnostic::new_error(
                "failed to write LLVM IR to file",
                Label::new(self.file_id, Span::dummy(), &format!("{}", e)),
            )
        })
    }

    fn compile(
        sess: &'gen Opts,
        file_id: FileId,
        ty_sess: &'gen ty::TySess,
        ir: &'gen ir::Ir,
    ) -> Result<()> {
        let context = Context::create();
        let module = context.create_module("mmtn");
        let builder = context.create_builder();
        module.set_source_file_name(sess.src_file_name());
        let ctx = GenSess {
            sess,
            file_id,
            context: &context,
            builder: &builder,
            module: &module,
            ty_sess,
            ir,
        };
        ctx.bind_library_fns();
        for def in ir.defs.values() {
            ctx.bind_def(def);
        }
        for def in ir.defs.values() {
            let compiled_def = ctx.compile_def(def)?;
            if ctx.sess.verify_llvm && !compiled_def.verify(false) {
                compiled_def.print_to_stderr();
                compiled_def.verify(true);
            }
        }
        if ctx.sess.dump_llvm {
            module.print_to_stderr();
        }
        module.verify().map_err(|err| {
            Diagnostic::new_bug(
                "LLVM IR could not be verified",
                Label::new(file_id, Span::dummy(), &format!("{}", err)),
            )
        })?;
        ctx.write_to_ll_file(&ctx.sess.dest)
    }
}

pub fn compile<'a>(
    sess: &'a Opts,
    file_id: FileId,
    ty_sess: &'a ty::TySess,
    ir: &'a ir::Ir,
) -> Result<()> {
    GenSess::compile(sess, file_id, ty_sess, ir)
}
