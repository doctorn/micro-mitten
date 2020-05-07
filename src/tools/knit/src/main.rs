use std::error::Error;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{self, Output};

use structopt::StructOpt;

use termion::color;

const EXIT_SUCCESS: i32 = 0;
const EXIT_FAILURE: i32 = 1;

#[derive(StructOpt, Debug)]
#[structopt(name = "mitten-test")]
pub struct Opts {
    #[structopt(long, parse(from_os_str), default_value = "mmtnc")]
    pub build: PathBuf,
    #[structopt(long = "src", short = "c", parse(from_os_str))]
    pub src: PathBuf,
    #[structopt(long, short = "v")]
    pub verbose: bool,
    #[structopt(long = "gc-strategy", default_value = "none")]
    pub gc_strategy: mitten_util::GcStrategy,
    #[structopt(short = "-")]
    pub mmtnc_args: Vec<String>,
}

#[derive(Debug)]
struct Failure {
    path: PathBuf,
    kind: FailureKind,
    cause: Option<Box<dyn Error + 'static>>,
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.path.display())?;
        writeln!(f, "{}", self.kind)?;
        if let Some(cause) = &self.cause {
            writeln!(f, "{}", cause)
        } else {
            Ok(())
        }
    }
}

impl Error for Failure {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self.cause {
            Some(ref cause) => Some(&**cause),
            _ => None,
        }
    }
}

#[derive(Debug)]
enum FailureKind {
    Fatal,
    ClangError(Output),
    MmtncError(Output),
}

impl fmt::Display for FailureKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FailureKind::Fatal => write!(f, "a fatal error occured"),
            FailureKind::MmtncError(output) => {
                writeln!(f, "compilation failed")?;
                writeln!(f, "stdout:")?;
                writeln!(f, "{}", String::from_utf8(output.stdout.clone()).unwrap())?;
                writeln!(f, "stderr:")?;
                write!(f, "{}", String::from_utf8(output.stderr.clone()).unwrap())
            }
            FailureKind::ClangError(output) => {
                writeln!(f, "a clang error occured")?;
                writeln!(f, "stdout:")?;
                writeln!(f, "{}", String::from_utf8(output.stdout.clone()).unwrap())?;
                writeln!(f, "stderr:")?;
                write!(f, "{}", String::from_utf8(output.stderr.clone()).unwrap())
            }
        }
    }
}

impl FailureKind {
    fn with_path_and_cause(self, path: &Path, cause: Box<dyn Error + 'static>) -> Failure {
        Failure {
            kind: self,
            path: path.to_owned(),
            cause: Some(cause),
        }
    }

    fn with_path(self, path: &Path) -> Failure {
        Failure {
            kind: self,
            path: path.to_owned(),
            cause: None,
        }
    }
}

fn build(opts: &Opts, path: &Path) -> Result<(), Failure> {
    // compile ll
    mitten_util::ll_to_bin(
        &path,
        opts.verbose,
        opts.gc_strategy,
        true,
        &|err| FailureKind::Fatal.with_path_and_cause(path, Box::new(err)),
        |output, bin| {
            if !output.status.success() {
                Err(FailureKind::ClangError(output).with_path(&opts.src))
            } else {
                fs::copy(bin, opts.src.file_stem().unwrap()).map_err(|err| {
                    FailureKind::Fatal.with_path_and_cause(&opts.src, Box::new(err))
                })?;
                Ok(())
            }
        },
    )
}

fn compile(opts: &Opts) -> Result<(), Failure> {
    // compile mmtn
    mitten_util::mmtn_to_ll(
        &opts.build,
        &opts.src,
        opts.verbose,
        opts.gc_strategy,
        opts.mmtnc_args.as_slice(),
        &|err| FailureKind::Fatal.with_path_and_cause(&opts.src, Box::new(err)),
        |output, ll| {
            if !output.status.success() {
                Err(FailureKind::MmtncError(output).with_path(&opts.src))
            } else {
                build(opts, ll)
            }
        },
    )
}

fn driver(opts: &Opts) -> i32 {
    match compile(opts) {
        Ok(_) => {
            println!(
                "knitted your binary! {}<3{}",
                color::Fg(color::Red),
                color::Fg(color::Reset)
            );
            EXIT_SUCCESS
        }
        Err(err) => {
            println!(
                "couldn't knit your binary {}</3{}",
                color::Fg(color::Red),
                color::Fg(color::Reset)
            );
            println!("{}", err);
            EXIT_FAILURE
        }
    }
}

fn main() {
    let opts = Opts::from_args();
    let exit_code = driver(&opts);
    println!("exiting with status code {}", exit_code);
    process::exit(exit_code);
}
