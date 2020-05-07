use crate::middle::analysis::{Analysis, DataFlow};
use crate::middle::ir;
use crate::pp;

// NOTE this is a very hacky way of pretty printing analysis results as it reduplicates a whole
// bunch of code that already lives in `super::pp` - these definitely need to be unified at some
// point...

impl<T: DataFlow> pp::PrettyPrintable for (&ir::Arm, &Analysis<T>) {
    fn pp(&self, printer: &mut pp::Printer) {
        match &self.0.pattern {
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
            .pp(&(&*self.0.target, self.1))
            .unindent()
            .write("},");
    }
}

impl<T: DataFlow> pp::PrettyPrintable for (&ir::Terminator, &Analysis<T>) {
    fn pp(&self, printer: &mut pp::Printer) {
        match self.0 {
            ir::Terminator::Return(idx) => printer.write("ret ").pp(idx),
            ir::Terminator::Match { source, arms } => {
                printer.write("match ").pp(source).write(" {").indent();
                for (i, arm) in arms.iter().enumerate() {
                    printer.pp(&(arm, self.1));
                    if i + 1 < arms.len() {
                        printer.newline();
                    }
                }
                printer.unindent().write("}")
            }
        };
    }
}

impl<T: DataFlow> pp::PrettyPrintable for (&ir::Block, &Analysis<T>) {
    fn pp(&self, printer: &mut pp::Printer) {
        let decoration = self.1.decoration(self.0);
        printer.writeln(&format!("// {:?}", decoration.carry));
        for (instruction, decoration) in self
            .0
            .instructions
            .iter()
            .zip(decoration.instructions.iter())
        {
            printer
                .pp(instruction)
                .newline()
                .writeln(&format!("// {:?}", decoration));
        }
        printer.pp(&(&self.0.terminator, self.1));
    }
}

impl<T: DataFlow> pp::PrettyPrintable for (&ir::Entry, &Analysis<T>) {
    fn pp(&self, printer: &mut pp::Printer) {
        for (idx, binding) in self.0.param_bindings.iter() {
            printer.pp(binding).write(" <- ").pp(&idx).newline();
        }
        printer.pp(&(&self.0.body, self.1));
    }
}

impl<T: DataFlow> pp::PrettyPrintable for (&ir::Def, &Analysis<T>) {
    fn pp(&self, printer: &mut pp::Printer) {
        printer
            .writeln(&format!(
                "// (context) {:?}",
                &*self.1.context(self.0.def_idx)
            ))
            .pp(&self.0.def_idx)
            .write(" {")
            .indent()
            .writeln(&format!(
                "// (summary) {:?}",
                &*self.1.summary(self.0.def_idx)
            ))
            .pp(&(&self.0.entry, self.1))
            .unindent()
            .writeln("}");
    }
}

impl<T: DataFlow> pp::PrettyPrintable for (&ir::Ir, &Analysis<T>) {
    fn pp(&self, printer: &mut pp::Printer) {
        for def in self.0.defs.values() {
            printer.pp(&(def, self.1));
        }
    }
}
