use codespan_reporting::term::ColorArg;

use structopt::clap::arg_enum;

use std::path::PathBuf;

arg_enum! {
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub enum GcStrategy {
        Proust,
        BDW,
        None,
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "mmtnc")]
pub struct Opts {
    #[structopt(short = "c", long, parse(from_os_str))]
    pub src: PathBuf,
    #[structopt(short = "o", long, parse(from_os_str), default_value = "mmtn.ll")]
    pub dest: PathBuf,
    #[structopt(long = "dump-trees")]
    pub dump_trees: bool,
    #[structopt(long = "pretty-print")]
    pub pretty_print: bool,
    #[structopt(long = "dump-ir")]
    pub dump_ir: bool,
    #[structopt(long = "dump-llvm")]
    pub dump_llvm: bool,
    #[structopt(long = "run-lva")]
    pub run_lva: bool,
    #[structopt(long = "dump-analyses")]
    pub dump_analyses: bool,
    #[structopt(long = "verify-llvm")]
    pub verify_llvm: bool,
    #[structopt(
        long = "gc-strategy",
        default_value = "none",
        raw(possible_values = "&GcStrategy::variants()", case_insensitive = "true")
    )]
    pub gc_strategy: GcStrategy,
    #[structopt(
        long = "color",
        parse(try_from_str),
        default_value = "auto",
        raw(possible_values = "ColorArg::VARIANTS", case_insensitive = "true")
    )]
    pub color: ColorArg,
}

impl Opts {
    pub fn src_file_name(&self) -> &str {
        self.src.to_str().unwrap_or("[FATAL]")
    }
}
