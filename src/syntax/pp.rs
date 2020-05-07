use crate::pp;
use crate::syntax::ast;
use crate::syntax::parse::{Parser, Restriction};

use std::ops::{Deref, DerefMut};
use std::u32;

struct ExprPrinter<'a> {
    prec: Vec<u32>,
    restriction: Restriction,
    printer: &'a mut pp::Printer,
}

impl<'a> Deref for ExprPrinter<'a> {
    type Target = pp::Printer;

    fn deref(&self) -> &Self::Target {
        &self.printer
    }
}

impl<'a> DerefMut for ExprPrinter<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.printer
    }
}

impl<'a> ExprPrinter<'a> {
    const MIN_PREC: u32 = u32::MIN;
    const MAX_PREC: u32 = u32::MAX;

    fn push(&mut self, prec: u32) {
        self.prec.push(prec)
    }

    fn prec(&self) -> u32 {
        self.prec.last().copied().unwrap_or(0)
    }

    fn pop(&mut self) {
        self.prec.pop();
    }

    fn pp_expr(&mut self, expr: &ast::Expr) {
        let restriction = self.restriction;
        self.restriction = Restriction::None;
        match expr {
            ast::Expr::Literal(literal) => {
                self.write(&format!("{}", literal));
            }
            ast::Expr::Var(ident) => {
                self.write(&ident);
            }
            ast::Expr::Unop { kind, operand } => {
                self.push(Self::MAX_PREC);
                self.pp(&**kind);
                self.pp_expr(operand);
                self.pop();
            }
            ast::Expr::Binop { kind, left, right } => {
                let prec = Parser::precedence_of(**kind);
                let paren = self.prec() > prec;
                self.push(prec);
                if paren {
                    self.write("(");
                }
                self.pp_expr(left);
                self.write(" ").pp(&**kind).write(" ");
                self.pp_expr(right);
                if paren {
                    self.write(")");
                }
                self.pop();
            }
            ast::Expr::Call { target, args } => {
                self.push(Self::MIN_PREC);
                self.write(&target).write("(");
                match args.len() {
                    0 => {}
                    1 => {
                        self.pp_expr(&args[0]);
                    }
                    _ => {
                        self.indent();
                        for arg in args.iter() {
                            self.pp_expr(arg);
                            self.write(",").newline();
                        }
                        self.unindent();
                    }
                }
                self.write(")");
                self.pop();
            }
            ast::Expr::Variant {
                enum_name,
                discriminant,
                body,
            } => {
                self.push(Self::MIN_PREC);
                self.write(&enum_name)
                    .write("::")
                    .write(&discriminant)
                    .write("(");
                self.pp_expr(body);
                self.write(")");
                self.pop();
            }
            ast::Expr::Record {
                struct_name,
                fields,
            } => {
                if restriction == Restriction::NoStructLiteral {
                    self.write("(");
                }
                self.push(Self::MIN_PREC);
                self.write(&struct_name).write(" {");
                if !fields.is_empty() {
                    self.indent();
                    for (field_name, bound) in fields.iter() {
                        self.write(&field_name).write(": ");
                        self.pp_expr(bound);
                        self.write(",").newline();
                    }
                    self.unindent();
                }
                self.write("}");
                self.pop();
                if restriction == Restriction::NoStructLiteral {
                    self.write(")");
                }
            }
        }
    }

    fn pp<'b>(
        printer: &'b mut pp::Printer,
        expr: &ast::Expr,
        restriction: Restriction,
    ) -> &'b mut pp::Printer {
        let mut expr_printer = ExprPrinter {
            prec: vec![],
            restriction,
            printer,
        };
        expr_printer.pp_expr(expr);
        expr_printer.printer
    }
}

impl pp::PrettyPrintable for ast::Ty {
    fn pp(&self, printer: &mut pp::Printer) {
        match self {
            ast::Ty::U64 => printer.write("u64"),
            ast::Ty::TyName(ident) => printer.write(ident),
        };
    }
}

impl pp::PrettyPrintable for ast::UnopKind {
    fn pp(&self, printer: &mut pp::Printer) {
        printer.write(match self {
            ast::UnopKind::Not => "!",
        });
    }
}

impl pp::PrettyPrintable for ast::BinopKind {
    fn pp(&self, printer: &mut pp::Printer) {
        printer.write(match self {
            ast::BinopKind::Plus => "+",
            ast::BinopKind::Minus => "-",
            ast::BinopKind::Mul => "*",
            ast::BinopKind::Div => "/",
            ast::BinopKind::Less => "<",
            ast::BinopKind::Leq => "<=",
            ast::BinopKind::Greater => ">",
            ast::BinopKind::Geq => ">=",
            ast::BinopKind::Eq => "==",
            ast::BinopKind::Neq => "!=",
            ast::BinopKind::And => "&",
            ast::BinopKind::Or => "|",
            ast::BinopKind::Xor => "^",
            ast::BinopKind::LShift => "<<",
            ast::BinopKind::RShift => ">>",
        });
    }
}

impl pp::PrettyPrintable for ast::Pattern {
    fn pp(&self, printer: &mut pp::Printer) {
        match self {
            ast::Pattern::Literal(literal) => printer.write(&format!("{}", literal)),
            ast::Pattern::Ident(ident) => printer.write(ident),
            ast::Pattern::Variant {
                enum_name,
                discriminant,
                bound,
            } => printer
                .write(enum_name)
                .write("::")
                .write(discriminant)
                .write("(")
                .write(bound)
                .write(")"),
            ast::Pattern::Record {
                struct_name,
                fields,
            } => {
                printer.write(struct_name).write(" {");
                if !fields.is_empty() {
                    printer.indent();
                    for (field_name, bound) in fields.iter() {
                        printer
                            .write(field_name)
                            .write(": ")
                            .write(bound)
                            .write(",")
                            .newline();
                    }
                    printer.unindent();
                }
                printer.write("}")
            }
        };
    }
}

impl pp::PrettyPrintable for ast::Term {
    fn pp(&self, printer: &mut pp::Printer) {
        match self {
            ast::Term::Let {
                binder,
                annotation,
                expr,
                body,
            } => {
                printer.write("let ").write(&*binder);
                if let Some(ty) = annotation {
                    printer.write(": ").pp(&**ty);
                }
                printer.write(" = ");
                ExprPrinter::pp(printer, &**expr, Restriction::None)
                    .write(";")
                    .newline()
                    .pp(&***body)
            }
            ast::Term::Match { source, arms } => {
                printer.write("match ");
                ExprPrinter::pp(printer, &**source, Restriction::NoStructLiteral)
                    .write(" {")
                    .indent();
                for (pattern, body) in arms.iter() {
                    printer.pp(&**pattern).write(" => ");
                    if let ast::Term::Let { .. } = &***body {
                        printer
                            .write("{")
                            .indent()
                            .pp(&***body)
                            .unindent()
                            .write("}");
                    } else {
                        printer.pp(&***body).write(",").newline();
                    }
                }
                printer.unindent().write("}")
            }
            ast::Term::If {
                source,
                then,
                otherwise,
            } => {
                printer.write("if ");
                ExprPrinter::pp(printer, source, Restriction::NoStructLiteral)
                    .write(" {")
                    .indent()
                    .pp(&***then)
                    .unindent()
                    .write("} else {")
                    .indent()
                    .pp(&***otherwise)
                    .unindent()
                    .write("}")
            }
            ast::Term::Return(expr) => ExprPrinter::pp(printer, expr, Restriction::None),
        };
    }
}

impl pp::PrettyPrintable for ast::FnDecl {
    fn pp(&self, printer: &mut pp::Printer) {
        printer
            .write("fn ")
            .write(&self.name)
            .write(" -> ")
            .pp(&*self.return_ty)
            .write(" {")
            .indent()
            .pp(&*self.body)
            .unindent()
            .write("}")
            .newline();
    }
}

impl pp::PrettyPrintable for ast::Enum {
    fn pp(&self, printer: &mut pp::Printer) {
        printer
            .write("enum ")
            .write(&self.name)
            .write(" {")
            .indent();
        for variant in self.variants.iter() {
            printer
                .write(&variant.binder)
                .write("(")
                .pp(&*variant.ty)
                .write("),")
                .newline();
        }
        printer.unindent().write("}").newline();
    }
}

impl pp::PrettyPrintable for ast::Struct {
    fn pp(&self, printer: &mut pp::Printer) {
        printer
            .write("struct ")
            .write(&self.name)
            .write(" {")
            .indent();
        for field in self.fields.iter() {
            printer
                .write(&field.binder)
                .write(": ")
                .pp(&*field.ty)
                .write(",")
                .newline();
        }
        printer.unindent().write("}").newline();
    }
}

impl pp::PrettyPrintable for ast::Item {
    fn pp(&self, printer: &mut pp::Printer) {
        match self {
            ast::Item::Fn(decl) => printer.pp(&**decl),
            ast::Item::Enum(enum_item) => printer.pp(enum_item),
            ast::Item::Struct(struct_item) => printer.pp(struct_item),
        };
    }
}

impl pp::PrettyPrintable for ast::Ast {
    fn pp(&self, printer: &mut pp::Printer) {
        for item in self.items.iter() {
            printer.pp(&**item).newline();
        }
    }
}
