use crate::middle::ir;
use crate::{pp, ty};

impl pp::PrettyPrintable for ir::LocalIdx {
    fn pp(&self, printer: &mut pp::Printer) {
        printer.write(&format!("{:?}", self));
    }
}

impl pp::PrettyPrintable for ir::DefIdx {
    fn pp(&self, printer: &mut pp::Printer) {
        printer.write(&format!("{:?}", self));
    }
}

impl pp::PrettyPrintable for ty::VariantIdx {
    fn pp(&self, printer: &mut pp::Printer) {
        printer.write(&format!("{:?}", self));
    }
}

impl pp::PrettyPrintable for ty::FieldIdx {
    fn pp(&self, printer: &mut pp::Printer) {
        printer.write(&format!("{:?}", self));
    }
}

impl pp::PrettyPrintable for ty::ParamIdx {
    fn pp(&self, printer: &mut pp::Printer) {
        printer.write(&format!("{:?}", self));
    }
}

impl pp::PrettyPrintable for ir::UnopKind {
    fn pp(&self, printer: &mut pp::Printer) {
        printer.write(match self {
            ir::UnopKind::Not => "!",
        });
    }
}

impl pp::PrettyPrintable for ir::BinopKind {
    fn pp(&self, printer: &mut pp::Printer) {
        printer.write(match self {
            ir::BinopKind::Plus => "+",
            ir::BinopKind::Minus => "-",
            ir::BinopKind::Mul => "*",
            ir::BinopKind::Div => "/",
            ir::BinopKind::Less => "<",
            ir::BinopKind::Leq => "<=",
            ir::BinopKind::Greater => ">",
            ir::BinopKind::Geq => ">=",
            ir::BinopKind::Eq => "==",
            ir::BinopKind::Neq => "!=",
            ir::BinopKind::And => "&",
            ir::BinopKind::Or => "|",
            ir::BinopKind::Xor => "^",
            ir::BinopKind::LShift => "<<",
            ir::BinopKind::RShift => ">>",
        });
    }
}

impl pp::PrettyPrintable for ir::Expr {
    fn pp(&self, printer: &mut pp::Printer) {
        match &self.kind {
            ir::ExprKind::Literal(literal) => printer.write(&format!("{}", literal)),
            ir::ExprKind::Var(idx) => printer.pp(idx),
            ir::ExprKind::Unop { kind, operand } => printer.pp(kind).pp(operand),
            ir::ExprKind::Binop { kind, left, right } => {
                printer.pp(left).write(" ").pp(kind).write(" ").pp(right)
            }
            ir::ExprKind::Call { target, args } => {
                target.pp(printer);
                printer.write("(");
                for (_, arg) in args.iter() {
                    printer.pp(arg).write(", ");
                }
                printer.write(")")
            }
            ir::ExprKind::Variant {
                discriminant, body, ..
            } => printer.pp(discriminant).write("(").pp(body).write(")"),
            ir::ExprKind::Record { fields, .. } => {
                printer.write("{");
                for (field_idx, local_idx) in fields.iter() {
                    printer.pp(&field_idx).write(": ").pp(local_idx).write(", ");
                }
                printer.write("}")
            }
        };
    }
}

impl pp::PrettyPrintable for ir::Arm {
    fn pp(&self, printer: &mut pp::Printer) {
        match &self.pattern {
            ir::PatternKind::Literal(literal) => printer.write(&format!("{}", literal)),
            ir::PatternKind::Ident(binding) => printer.pp(binding),
            ir::PatternKind::Variant {
                discriminant,
                binding,
                ..
            } => printer.pp(discriminant).write("(").pp(binding).write(")"),
            ir::PatternKind::Record { fields, .. } => {
                printer.write("{");
                for (field_idx, binding) in fields.iter() {
                    printer.pp(&field_idx).write(": ").pp(binding).write(", ");
                }
                printer.write("}")
            }
        };
        printer
            .write(" => {")
            .indent()
            .pp(&*self.target)
            .unindent()
            .write("},");
    }
}

impl pp::PrettyPrintable for ir::Instruction {
    fn pp(&self, printer: &mut pp::Printer) {
        match &self.kind {
            ir::InstructionKind::Let { binding, expr, .. } => {
                printer.pp(binding).write(" <- ").pp(expr)
            }
            ir::InstructionKind::Mark(idx, _) => printer.write("mark ").pp(idx),
            ir::InstructionKind::Unmark(idx, _) => printer.write("unmark ").pp(idx),
            ir::InstructionKind::Free(idx, _) => printer.write("free ").pp(idx),
            ir::InstructionKind::RTReset => printer.write("rt_reset"),
        };
    }
}

impl pp::PrettyPrintable for ir::Block {
    fn pp(&self, printer: &mut pp::Printer) {
        for instruction in self.instructions.iter() {
            printer.pp(instruction).newline();
        }
        printer.pp(&self.terminator);
    }
}

impl pp::PrettyPrintable for ir::Terminator {
    fn pp(&self, printer: &mut pp::Printer) {
        match self {
            ir::Terminator::Return(idx) => printer.write("ret ").pp(idx),
            ir::Terminator::Match { source, arms } => {
                printer.write("match ").pp(source).write(" {").indent();
                for (i, arm) in arms.iter().enumerate() {
                    printer.pp(arm);
                    if i + 1 < arms.len() {
                        printer.newline();
                    }
                }
                printer.unindent().write("}")
            }
        };
    }
}

impl pp::PrettyPrintable for ir::Entry {
    fn pp(&self, printer: &mut pp::Printer) {
        for (idx, binding) in self.param_bindings.iter() {
            printer.pp(binding).write(" <- ").pp(&idx).newline();
        }
        printer.pp(&self.body);
    }
}

impl pp::PrettyPrintable for ir::Def {
    fn pp(&self, printer: &mut pp::Printer) {
        printer
            .pp(&self.def_idx)
            .write(" {")
            .indent()
            .pp(&self.entry)
            .unindent()
            .writeln("}");
    }
}

impl pp::PrettyPrintable for ir::Ir {
    fn pp(&self, printer: &mut pp::Printer) {
        for def in self.defs.values() {
            printer.pp(def);
        }
    }
}
