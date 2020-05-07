use crate::diagnostics::{self, Diagnostic, FileId, Files, Label, Result, Span};
use crate::{back, middle, syntax};

use std::fs::File;
use std::io::Read;
use std::process;
use std::time::Instant;

use structopt::StructOpt;

mod opts;

pub use opts::*;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub const EXIT_SUCCESS: i32 = 0;
pub const EXIT_FAILURE: i32 = 1;

fn open_src_file(opts: &Opts, files: &mut Files) -> Result<FileId> {
    // NOTE we should probably use less hacky approach to diagnostic printing here as we shouldn't
    // really be spanning errors on files that haven't been read
    match File::open(&opts.src) {
        Ok(mut file) => {
            let mut src = String::new();
            match file.read_to_string(&mut src) {
                Ok(_) => Ok(files.add(opts.src.to_str().unwrap().to_owned(), src.as_str())),
                Err(err) => {
                    let file_id = files.add(opts.src_file_name().to_owned(), "");
                    Err(Diagnostic::new_error(
                        "failed to read source file",
                        Label::new(file_id, Span::dummy(), err.to_string()),
                    ))
                }
            }
        }
        Err(err) => {
            let file_id = files.add(opts.src_file_name().to_owned(), "");
            Err(Diagnostic::new_error(
                "failed to open source file",
                Label::new(file_id, Span::dummy(), err.to_string()),
            ))
        }
    }
}

fn run_pipeline(opts: &Opts, files: &mut Files) -> Result<()> {
    let file_id = open_src_file(opts, files)?;
    let ast = syntax::parse(opts, files, file_id)?;
    let (ir, ty_sess) = middle::lower(opts, file_id, &ast)?;
    let ty_env = middle::ty_check(opts, file_id, &ty_sess, &ir)?;
    if opts.run_lva {
        let analysis_runtime = middle::analysis::runtime(opts, file_id, &ty_sess, &ty_env, &ir)?;
        analysis_runtime.pass::<middle::analysis::passes::Lva>();
    }
    let ir = if opts.gc_strategy == opts::GcStrategy::Proust {
        middle::transforms::clean(opts, file_id, &ty_sess, &ty_env, &ir)?
    } else {
        ir
    };
    back::compile(opts, file_id, &ty_sess, &ir)
}

fn init_env_logger() {
    env_logger::init_from_env("MMTNC_LOG");
    debug!("mmtnc v{}", VERSION);
}

fn parse_args() -> Opts {
    let opts = Opts::from_args();
    debug!("{:#?}", opts);
    opts
}

pub fn driver() {
    let start = Instant::now();
    init_env_logger();
    let opts = parse_args();
    let mut files = Files::new();
    let exit_code = match run_pipeline(&opts, &mut files) {
        Ok(_) => EXIT_SUCCESS,
        Err(diagnostic) => {
            diagnostics::emit(&opts, &files, &diagnostic);
            EXIT_FAILURE
        }
    };
    debug!(
        "compilation completed in {}ms (exit code {})",
        start.elapsed().as_millis(),
        exit_code
    );
    process::exit(exit_code);
}
