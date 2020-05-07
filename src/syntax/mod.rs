use crate::diagnostics::{FileId, Files, Result};
use crate::driver::Opts;

mod lex;
mod parse;
mod pp;
mod token;

pub mod ast;

pub fn parse(sess: &Opts, files: &Files, file_id: FileId) -> Result<ast::Ast> {
    let parser = parse::Parser::new(sess, files, file_id)?;
    let mut items = vec![];
    for item in parser {
        items.push(item?);
    }
    let ast = ast::Ast { items };
    if sess.dump_trees {
        for item in ast.items.iter() {
            println!("{:#?}", item);
        }
    }
    if sess.pretty_print {
        use crate::pp;
        pp!(&ast);
    }
    Ok(ast)
}
