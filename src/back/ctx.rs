use crate::diagnostics::{Diagnostic, Label, Result};
use crate::driver::GcStrategy;
use crate::middle::ir;
use crate::ty;

use inkwell::values::{BasicValueEnum, FunctionValue, IntValue};
use inkwell::IntPredicate;

use std::collections::HashMap;
use std::ops::Deref;

use super::GenSess;

macro_rules! local {
    ( $idx:expr ) => {{
        use $crate::common::Idx;
        &format!("mmtn_{}", $idx.index())
    }};
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum MatchCase<'ctx> {
    Wild,
    Record,
    Literal(IntValue<'ctx>),
    Variant(ty::Ty, IntValue<'ctx>),
}

pub(super) struct GenCtx<'gen, 'ctx> {
    sess: &'gen GenSess<'gen, 'ctx>,
    ir: &'gen ir::Def,
    llvm: FunctionValue<'ctx>,
    bindings: HashMap<ir::LocalIdx, BasicValueEnum<'ctx>>,
}

impl<'gen, 'ctx> GenCtx<'gen, 'ctx> {
    fn bind(&mut self, idx: ir::LocalIdx, value: BasicValueEnum<'ctx>) {
        self.bindings.insert(idx, value);
    }

    fn lookup(&self, idx: ir::LocalIdx) -> Result<BasicValueEnum<'ctx>> {
        if let Some(value) = self.bindings.get(&idx) {
            Ok(*value)
        } else {
            Err(Diagnostic::new_bug(
                "reference to unbound local index",
                Label::new(
                    self.file_id,
                    idx.span(),
                    &format!("'{:?}' not bound in this scope", idx),
                ),
            ))
        }
    }

    fn compile_unop(
        &self,
        idx: ir::LocalIdx,
        kind: ir::UnopKind,
        operand: ir::LocalIdx,
    ) -> Result<BasicValueEnum<'ctx>> {
        let operand = self.lookup(operand)?.into_int_value();
        Ok(match kind {
            ir::UnopKind::Not => self.builder.build_not(operand, local!(idx)).into(),
        })
    }

    fn compile_int_predicate(&self, kind: ir::BinopKind) -> Option<IntPredicate> {
        Some(match kind {
            ir::BinopKind::Less => IntPredicate::ULT,
            ir::BinopKind::Leq => IntPredicate::ULE,
            ir::BinopKind::Greater => IntPredicate::UGT,
            ir::BinopKind::Geq => IntPredicate::UGE,
            ir::BinopKind::Eq => IntPredicate::EQ,
            ir::BinopKind::Neq => IntPredicate::NE,
            _ => return None,
        })
    }

    fn compile_binop(
        &self,
        idx: ir::LocalIdx,
        kind: ir::BinopKind,
        left: ir::LocalIdx,
        right: ir::LocalIdx,
    ) -> Result<BasicValueEnum<'ctx>> {
        let left = self.lookup(left)?.into_int_value();
        let right = self.lookup(right)?.into_int_value();
        Ok(match kind {
            ir::BinopKind::Plus => self.builder.build_int_add(left, right, local!(idx)).into(),
            ir::BinopKind::Minus => self.builder.build_int_sub(left, right, local!(idx)).into(),
            ir::BinopKind::Mul => self.builder.build_int_mul(left, right, local!(idx)).into(),
            ir::BinopKind::Div => self
                .builder
                .build_int_unsigned_div(left, right, local!(idx))
                .into(),
            ir::BinopKind::And => self.builder.build_and(left, right, local!(idx)).into(),
            ir::BinopKind::Or => self.builder.build_or(left, right, local!(idx)).into(),
            ir::BinopKind::Xor => self.builder.build_xor(left, right, local!(idx)).into(),
            ir::BinopKind::LShift => self
                .builder
                .build_left_shift(left, right, local!(idx))
                .into(),
            ir::BinopKind::RShift => self
                .builder
                .build_right_shift(left, right, false, local!(idx))
                .into(),
            comparison => {
                let comparison = self.builder.build_int_compare(
                    self.compile_int_predicate(comparison).unwrap(),
                    left,
                    right,
                    local!(idx),
                );
                self.builder
                    .build_int_z_extend::<IntValue>(comparison, self.context.i64_type(), "cast_tmp")
                    .into()
            }
        })
    }

    fn compile_expr(&mut self, expr: &ir::Expr) -> Result<BasicValueEnum<'ctx>> {
        match &expr.kind {
            ir::ExprKind::Literal(literal) => {
                Ok(self.context.i64_type().const_int(*literal, false).into())
            }
            ir::ExprKind::Var(idx) => self.lookup(*idx),
            ir::ExprKind::Unop { kind, operand } => {
                self.compile_unop(expr.local_idx, *kind, *operand)
            }
            ir::ExprKind::Binop { kind, left, right } => {
                self.compile_binop(expr.local_idx, *kind, *left, *right)
            }
            ir::ExprKind::Call { target, args } => {
                let target_fn = self.lookup_def(*target, expr.span)?;
                let mut compiled_args = Vec::with_capacity(args.len());
                for arg in args.values() {
                    compiled_args.push(self.lookup(*arg)?);
                }
                self.builder
                    .build_call(target_fn, compiled_args.as_slice(), local!(expr.local_idx))
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| {
                        Diagnostic::new_bug(
                            "attempted to return non-basic value from function call",
                            Label::new(
                                self.file_id,
                                expr.span,
                                "this call returns a non-basic value",
                            ),
                        )
                    })
            }
            ir::ExprKind::Variant {
                ty,
                discriminant,
                body,
            } => {
                let variant = self.build_alloc(*ty, local!(expr.local_idx), expr.span)?;
                self.write_enum_discriminant(variant, *ty, *discriminant);
                self.write_enum_body(variant, *ty, *discriminant, self.lookup(*body)?);
                if self.sess.sess.gc_strategy == GcStrategy::Proust {
                    self.write_mark(variant, *ty, false);
                }
                Ok(variant.into())
            }
            ir::ExprKind::Record { ty, fields } => {
                let record = self.build_alloc(*ty, local!(expr.local_idx), expr.span)?;
                for (field_idx, local_idx) in fields.iter() {
                    self.write_struct_field(record, *ty, field_idx, self.lookup(*local_idx)?);
                }
                if self.sess.sess.gc_strategy == GcStrategy::Proust {
                    self.write_mark(record, *ty, false);
                }
                Ok(record.into())
            }
        }
    }

    fn compile_instruction(&mut self, instruction: &ir::Instruction) -> Result<()> {
        match &instruction.kind {
            ir::InstructionKind::Let { binding, expr, .. } => {
                let compiled_expr = self.compile_expr(expr)?;
                self.bind(*binding, compiled_expr);
            }
            ir::InstructionKind::Mark(idx, ty) => {
                // %a.mark := 1;
                self.write_mark(self.lookup(*idx)?.into_pointer_value(), *ty, true);
            }
            ir::InstructionKind::Unmark(idx, ty) => {
                // %a.mark := 0;
                self.write_mark(self.lookup(*idx)?.into_pointer_value(), *ty, false);
            }
            ir::InstructionKind::Free(idx, ty) => {
                // if marked(%a) {} else { @mitten_free(%a); }
                let ptr = self.lookup(*idx)?.into_pointer_value();
                let free_block = self.context.append_basic_block(self.llvm, "free");
                let merge_block = self.context.append_basic_block(self.llvm, "merge");
                self.builder.build_conditional_branch(
                    self.read_mark(ptr, *ty).into_int_value(),
                    merge_block,
                    free_block,
                );
                self.builder.position_at_end(free_block);
                self.build_free(ptr);
                self.builder.build_unconditional_branch(merge_block);
                self.builder.position_at_end(merge_block);
            }
            ir::InstructionKind::RTReset => {
                // @mitten_reset();
                self.build_reset();
            }
        }
        Ok(())
    }

    fn compile_block(&mut self, block: &ir::Block) -> Result<()> {
        for instruction in block.instructions.iter() {
            self.compile_instruction(instruction)?;
        }
        self.compile_terminator(&block.terminator)
    }

    fn compile_pattern(
        &mut self,
        source: BasicValueEnum<'ctx>,
        pattern: &ir::PatternKind,
    ) -> MatchCase<'ctx> {
        match pattern {
            ir::PatternKind::Literal(literal) => MatchCase::Literal(self.compile_literal(*literal)),
            ir::PatternKind::Ident(binding) => {
                self.bind(*binding, source);
                MatchCase::Wild
            }
            ir::PatternKind::Variant {
                ty,
                discriminant,
                binding,
            } => {
                self.bind(
                    *binding,
                    self.read_enum_body(source.into_pointer_value(), *ty, *discriminant),
                );
                MatchCase::Variant(*ty, self.compile_variant_idx(*discriminant))
            }
            ir::PatternKind::Record { fields, ty } => {
                let source = source.into_pointer_value();
                for (field_idx, binding) in fields.iter() {
                    self.bind(*binding, self.read_struct_field(source, *ty, field_idx));
                }
                MatchCase::Record
            }
        }
    }

    fn compile_terminator(&mut self, terminator: &ir::Terminator) -> Result<()> {
        match terminator {
            ir::Terminator::Return(local_idx) => {
                self.builder.build_return(Some(&self.lookup(*local_idx)?));
            }
            ir::Terminator::Match { source, arms } => {
                let source = self.lookup(*source)?;
                let origin = self.builder.get_insert_block().unwrap();
                let mut source_ty = None;
                let mut else_block = None;
                let mut cases = vec![];
                for (i, arm) in arms.iter().enumerate() {
                    let block = if let ir::PatternKind::Record { .. } = &arm.pattern {
                        origin
                    } else {
                        self.context
                            .append_basic_block(self.llvm, &format!("arm_{}", i))
                    };
                    self.builder.position_at_end(block);
                    match self.compile_pattern(source, &arm.pattern) {
                        MatchCase::Wild => {
                            else_block = Some(block);
                            self.compile_block(&arm.target)?;
                            break;
                        }
                        MatchCase::Record => {
                            self.compile_block(&arm.target)?;
                            return Ok(());
                        }
                        MatchCase::Literal(case) => {
                            cases.push((case, block));
                            self.compile_block(&arm.target)?;
                        }
                        MatchCase::Variant(ty, case) => {
                            source_ty = Some(ty);
                            cases.push((case, block));
                            self.compile_block(&arm.target)?;
                        }
                    }
                }
                let else_block = match else_block {
                    Some(block) => block,
                    _ => {
                        let block = self
                            .context
                            .append_basic_block(self.llvm, "unreachable_else");
                        self.builder.position_at_end(block);
                        self.builder.build_unreachable();
                        block
                    }
                };
                self.builder.position_at_end(origin);
                let source = if let Some(ty) = source_ty {
                    self.read_enum_discriminant(source.into_pointer_value(), ty)?
                } else {
                    source
                }
                .into_int_value();
                self.builder
                    .build_switch(source, else_block, cases.as_slice());
            }
        }
        Ok(())
    }

    fn compile_entry(&mut self, entry: &ir::Entry) -> Result<()> {
        let entry_block = self.context.append_basic_block(self.llvm, "entry");
        self.builder.position_at_end(entry_block);
        for (param_idx, binding) in entry.param_bindings.iter() {
            self.bind(*binding, self.read_param(self.llvm, param_idx));
        }
        self.compile_block(&entry.body)
    }

    fn compile(mut self) -> Result<FunctionValue<'ctx>> {
        self.compile_entry(&self.ir.entry)?;
        Ok(self.llvm)
    }

    pub(super) fn compile_def(
        sess: &'gen GenSess<'gen, 'ctx>,
        def: &'gen ir::Def,
    ) -> Result<FunctionValue<'ctx>> {
        let ctx = GenCtx {
            sess,
            ir: def,
            llvm: sess.lookup_def(def.def_idx, def.span)?,
            bindings: HashMap::new(),
        };
        ctx.compile()
    }
}

impl<'gen, 'ctx> Deref for GenCtx<'gen, 'ctx> {
    type Target = GenSess<'gen, 'ctx>;

    fn deref(&self) -> &Self::Target {
        self.sess
    }
}
