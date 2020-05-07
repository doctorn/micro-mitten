use serde::Serialize;

use std::path::Path;
use std::process::{Command, Output};
use std::{fs, io};

use structopt::clap::arg_enum;

pub const EXT: &str = "mmtn";

const VALGRIND: &str = "valgrind";

arg_enum! {
    #[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize)]
    pub enum GcStrategy {
        None,
        BDW,
        Proust,
    }
}

pub fn visit_dirs(dir: &Path, cb: &mut impl FnMut(&Path) -> io::Result<()>) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb)?;
            } else {
                cb(&path)?;
            }
        }
        Ok(())
    } else {
        cb(dir)
    }
}

pub fn mmtn_to_ll<T, E>(
    mmtnc: &Path,
    src: &Path,
    verbose: bool,
    gc_strategy: GcStrategy,
    compile_flags: &[String],
    io_handler: &impl Fn(io::Error) -> E,
    output_handler: impl FnOnce(Output, &Path) -> Result<T, E>,
) -> Result<T, E> {
    let temp = tempfile::NamedTempFile::new().map_err(io_handler)?;
    let dest = temp.into_temp_path();
    let mut compile = Command::new(&mmtnc);
    compile
        .arg("-c")
        .arg(&src)
        .arg("-o")
        .arg(&dest)
        .arg("--gc-strategy")
        .arg(&format!("{}", gc_strategy));
    for flag in compile_flags.iter() {
        compile.arg(flag);
    }
    if verbose {
        println!("{:?}", compile);
    }
    let output = compile.output().map_err(io_handler)?;
    output_handler(output, &dest)
}

pub fn ll_to_bin<T, E>(
    src: &Path,
    verbose: bool,
    gc_strategy: GcStrategy,
    optimise: bool,
    io_handler: &impl Fn(io::Error) -> E,
    output_handler: impl FnOnce(Output, &Path) -> Result<T, E>,
) -> Result<T, E> {
    let temp = tempfile::NamedTempFile::new().map_err(io_handler)?;
    let dest = temp.into_temp_path();
    let mut clang = Command::new("clang");
    clang.arg("-xir").arg(&src).arg("-o").arg(&dest);
    if optimise {
        clang.arg("-O3");
    } else {
        clang.arg("-O0");
    }
    match gc_strategy {
        GcStrategy::BDW => {
            clang.arg("-lgc");
        }
        GcStrategy::Proust => {
            clang
                .arg("-Wl,--gc-sections")
                .arg("-ldl")
                .arg("-lpthread")
                .arg("-lmitten_rt");
        }
        _ => {}
    }
    if verbose {
        println!("{:?}", clang);
    }
    let output = clang.output().map_err(io_handler)?;
    output_handler(output, &dest)
}

pub fn run<T, E>(
    bin: &Path,
    verbose: bool,
    valgrind_tool: Option<&str>,
    valgrind_options: Vec<&str>,
    io_handler: &impl Fn(io::Error) -> E,
    output_handler: impl FnOnce(Output) -> Result<T, E>,
) -> Result<T, E> {
    let mut run = if let Some(valgrind_tool) = valgrind_tool {
        let mut valgrind = Command::new(VALGRIND);
        valgrind.arg(&format!("--tool={}", valgrind_tool));
        for option in valgrind_options.iter() {
            valgrind.arg(option);
        }
        valgrind.arg(bin);
        valgrind
    } else {
        Command::new(bin)
    };
    if verbose {
        println!("{:?}", run);
    }
    run.output().map_err(io_handler).and_then(output_handler)
}
