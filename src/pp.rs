#[derive(Default)]
pub struct Printer {
    waiting_spaces: usize,
    indent: usize,
    out: String,
}

impl Printer {
    pub fn new() -> Printer {
        Printer::default()
    }

    fn flush_spaces(&mut self) {
        for _ in 0..self.waiting_spaces {
            self.out += "    ";
        }
        self.waiting_spaces = 0;
    }

    #[inline]
    pub fn newline(&mut self) -> &mut Printer {
        self.out += "\n";
        self.waiting_spaces = self.indent;
        self
    }

    #[inline]
    pub fn write(&mut self, line: &str) -> &mut Printer {
        self.flush_spaces();
        self.out += line;
        self
    }

    #[inline]
    pub fn writeln(&mut self, line: &str) -> &mut Printer {
        self.write(line).newline()
    }

    #[inline]
    pub fn indent(&mut self) -> &mut Printer {
        self.indent = self.indent.saturating_add(1);
        if self.waiting_spaces == 0 {
            self.newline()
        } else {
            self.waiting_spaces = self.indent;
            self
        }
    }

    #[inline]
    pub fn unindent(&mut self) -> &mut Printer {
        self.indent = self.indent.saturating_sub(1);
        if self.waiting_spaces == 0 {
            self.newline()
        } else {
            self.waiting_spaces = self.indent;
            self
        }
    }

    #[inline]
    pub fn pp<T: PrettyPrintable>(&mut self, t: &T) -> &mut Printer {
        t.pp(self);
        self
    }

    #[inline]
    pub fn log(self) {
        print!("{}", self.out)
    }
}

pub trait PrettyPrintable {
    fn pp(&self, printer: &mut Printer);
}

#[macro_export]
macro_rules! pp {
    ( $expr:expr ) => {{
        let mut printer = $crate::pp::Printer::new();
        printer.pp($expr);
        printer.log();
    }};
}
