#[macro_use]
extern crate log;
#[macro_use]
extern crate structopt;

pub mod back;
pub mod common;
pub mod driver;
pub mod middle;
pub mod pp;
pub mod syntax;
pub mod ty;

pub use driver::driver;

pub mod diagnostics {
    use crate::driver::Opts;

    use std::ops::Deref;

    use codespan::ByteIndex;

    pub use codespan::{FileId, Files};
    pub use codespan_reporting::diagnostic::{Diagnostic, Label};

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub struct Span(codespan::Span);

    #[derive(Debug)]
    pub struct Spanned<T>(Span, T);

    impl Into<codespan::Span> for Span {
        #[inline]
        fn into(self) -> codespan::Span {
            self.0
        }
    }

    impl From<codespan::Span> for Span {
        #[inline]
        fn from(span: codespan::Span) -> Span {
            Span(span)
        }
    }

    impl Deref for Span {
        type Target = codespan::Span;

        #[inline]
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl Span {
        #[inline]
        pub fn new(lo: impl Into<ByteIndex>, hi: impl Into<ByteIndex>) -> Span {
            codespan::Span::new(lo, hi).into()
        }

        #[inline]
        pub fn dummy() -> Span {
            codespan::Span::initial().into()
        }

        #[inline]
        pub fn span<T>(self, t: T) -> Spanned<T> {
            Spanned(self, t)
        }

        #[inline]
        pub fn clip(self) -> Span {
            codespan::Span::new(self.0.end(), self.0.end()).into()
        }

        #[inline]
        pub fn merge(self, other: Span) -> Span {
            self.0.merge(other.0).into()
        }
    }

    impl<T> Spanned<T> {
        #[inline]
        pub fn into_raw(self) -> T {
            self.1
        }

        #[inline]
        pub fn span(&self) -> Span {
            self.0
        }

        #[inline]
        pub fn respan(self, span: Span) -> Spanned<T> {
            span.span(self.1)
        }

        #[inline]
        pub fn boxed(self) -> Spanned<Box<T>> {
            self.0.span(Box::new(self.1))
        }
    }

    impl<T> Deref for Spanned<T> {
        type Target = T;

        #[inline]
        fn deref(&self) -> &Self::Target {
            &self.1
        }
    }

    pub type Result<T> = std::result::Result<T, Diagnostic>;

    pub fn emit(opts: &Opts, files: &Files, diagnostic: &Diagnostic) {
        use codespan_reporting::term;
        let writer = term::termcolor::StandardStream::stderr(opts.color.into());
        let config = term::Config::default();
        debug!("trying to emit diagnostic {:?}", diagnostic);
        term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
    }
}
