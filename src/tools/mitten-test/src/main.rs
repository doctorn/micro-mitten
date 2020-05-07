use serde::Deserialize;

use std::collections::HashMap;
use std::error::Error;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{self, Read as _, Write as _};
use std::path::{Path, PathBuf};
use std::process::{self, Output};
use std::{env, fmt};

use structopt::StructOpt;

use termion::color;

const EXIT_SUCCESS: i32 = 0;
const EXIT_FAILURE: i32 = 1;

#[derive(StructOpt, Debug)]
#[structopt(name = "mitten-test")]
pub struct Opts {
    #[structopt(long, parse(from_os_str), default_value = "mmtnc")]
    pub build: PathBuf,
    #[structopt(long = "path", parse(from_os_str), default_value = "./")]
    pub test_path: PathBuf,
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
    CompileStatusMismatch(Output),
    ClangError(Output),
    ExpectationMismatch { expected: i32, actual: i32 },
    NoStatusCode(Output),
    ArgumentParseFailure(String),
}

impl fmt::Display for FailureKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FailureKind::Fatal => write!(f, "a fatal error occured"),
            FailureKind::CompileStatusMismatch(output) => {
                if output.status.success() {
                    write!(f, "compilation succeeded when it was expected to fail")
                } else {
                    writeln!(f, "compilation failed when it was expected to succeed")?;
                    writeln!(f, "stdout:")?;
                    writeln!(f, "{}", String::from_utf8(output.stdout.clone()).unwrap())?;
                    writeln!(f, "stderr:")?;
                    write!(f, "{}", String::from_utf8(output.stderr.clone()).unwrap())
                }
            }
            FailureKind::ClangError(output) => {
                writeln!(f, "a clang error occured")?;
                writeln!(f, "stdout:")?;
                writeln!(f, "{}", String::from_utf8(output.stdout.clone()).unwrap())?;
                writeln!(f, "stderr:")?;
                write!(f, "{}", String::from_utf8(output.stderr.clone()).unwrap())
            }
            FailureKind::ExpectationMismatch { expected, actual } => {
                write!(f, "expected result was {}, but got {}", expected, actual)
            }
            FailureKind::NoStatusCode(output) => {
                writeln!(f, "could not read status code for test")?;
                writeln!(f, "stdout:")?;
                writeln!(f, "{}", String::from_utf8(output.stdout.clone()).unwrap())?;
                writeln!(f, "stderr:")?;
                write!(f, "{}", String::from_utf8(output.stderr.clone()).unwrap())
            }
            FailureKind::ArgumentParseFailure(reason) => {
                write!(f, "failed to parse test arguments because {}", reason)
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

fn collect_tests(opts: &Opts) -> Result<Vec<PathBuf>, Failure> {
    let mut tests = vec![];
    let mmtn_extension = OsStr::new(mitten_util::EXT);
    mitten_util::visit_dirs(&opts.test_path, &mut |path| {
        if let Some(extension) = path.extension() {
            if extension == mmtn_extension {
                if opts.verbose {
                    println!("found {}", &path.display());
                }
                tests.push(path.to_owned());
            }
        }
        Ok(())
    })
    .map_err(|err| FailureKind::Fatal.with_path_and_cause(&opts.test_path, Box::new(err)))?;
    println!(
        "found {}{}{} test file(s)",
        color::Fg(color::Green),
        tests.len(),
        color::Fg(color::Reset),
    );
    Ok(tests)
}

#[derive(Copy, Clone, PartialEq, Eq, Deserialize)]
enum CompileStatus {
    #[serde(rename = "ok")]
    Ok,
    #[serde(rename = "fail")]
    Fail,
}

#[derive(Deserialize)]
struct TestArgs {
    compile_status: CompileStatus,
    expectation: Option<i32>,
    compile_flags: Option<Vec<String>>,
    #[serde(default)]
    ignore: bool,
    env: Option<HashMap<String, String>>,
}

fn parse_test(path: &Path) -> Result<TestArgs, Failure> {
    let mut test_src = String::new();
    let mut file = File::open(path)
        .map_err(|err| FailureKind::Fatal.with_path_and_cause(path, Box::new(err)))?;
    file.read_to_string(&mut test_src)
        .map_err(|err| FailureKind::Fatal.with_path_and_cause(path, Box::new(err)))?;
    let unparsed = test_src.lines().next().ok_or(
        FailureKind::ArgumentParseFailure("file appears to be empty".to_string()).with_path(path),
    )?;
    serde_json::from_str(&unparsed[3..]).map_err(|err| {
        FailureKind::ArgumentParseFailure("the argument json could not be parsed".to_string())
            .with_path_and_cause(path, Box::new(err))
    })
}

fn run_test(opts: &Opts, test: &Path, test_args: &TestArgs, path: &Path) -> Result<(), Failure> {
    // run
    mitten_util::run(
        path,
        opts.verbose,
        None,
        vec![],
        &|err| FailureKind::Fatal.with_path_and_cause(path, Box::new(err)),
        |output| {
            if let Some(expected) = test_args.expectation {
                match output.status.code() {
                    Some(actual) if actual != expected % 256 => {
                        Err(FailureKind::ExpectationMismatch { expected, actual }.with_path(path))
                    }
                    None => Err(FailureKind::NoStatusCode(output).with_path(test)),
                    _ => Ok(()),
                }
            } else {
                Ok(())
            }
        },
    )
}

fn build_test(opts: &Opts, test: &Path, test_args: &TestArgs, path: &Path) -> Result<(), Failure> {
    // compile ll
    mitten_util::ll_to_bin(
        &path,
        opts.verbose,
        opts.gc_strategy,
        true,
        &|err| FailureKind::Fatal.with_path_and_cause(test, Box::new(err)),
        |output, bin| {
            if !output.status.success() {
                Err(FailureKind::ClangError(output).with_path(test))
            } else {
                run_test(opts, test, test_args, bin)
            }
        },
    )
}

fn compile_test(opts: &Opts, path: &Path) -> Result<(), Failure> {
    // collect flags
    let mut test_args = parse_test(path)?;
    if let Some(ref env) = test_args.env {
        for (key, value) in env.iter() {
            env::set_var(key, value);
        }
    }
    let mut compile_flags = opts.mmtnc_args.clone();
    if let Some(ref mut flags) = test_args.compile_flags {
        compile_flags.append(flags);
    }
    // compile mmtn
    let result = mitten_util::mmtn_to_ll(
        &opts.build,
        path,
        opts.verbose,
        opts.gc_strategy,
        compile_flags.as_slice(),
        &|err| FailureKind::Fatal.with_path_and_cause(path, Box::new(err)),
        |output, ll| match test_args.compile_status {
            CompileStatus::Ok if !output.status.success() => {
                Err(FailureKind::CompileStatusMismatch(output).with_path(path))
            }
            CompileStatus::Fail if output.status.success() => {
                Err(FailureKind::CompileStatusMismatch(output).with_path(path))
            }
            CompileStatus::Fail => Ok(()),
            _ => build_test(opts, path, &test_args, ll),
        },
    );
    if test_args.ignore && result.is_err() {
        print!(
            "({}ignored{}) ",
            color::Fg(color::Yellow),
            color::Fg(color::Reset)
        );
        Ok(())
    } else {
        result
    }
}

fn run_tests(opts: &Opts, tests: &Vec<PathBuf>) -> Vec<Failure> {
    let mut failures = vec![];
    for test in tests.iter() {
        print!("running {}... ", test.display());
        io::stdout().flush().unwrap();
        if opts.verbose {
            println!();
        }
        if let Err(failure) = compile_test(opts, test) {
            failures.push(failure);
            println!("{}FAIL{}", color::Fg(color::Red), color::Fg(color::Reset));
        } else {
            println!("{}OK{}", color::Fg(color::Green), color::Fg(color::Reset));
        }
    }
    failures
}

fn driver(opts: &Opts) -> i32 {
    let tests = match collect_tests(&opts) {
        Ok(tests) => tests,
        Err(failure) => {
            println!("{}", failure);
            return EXIT_FAILURE;
        }
    };
    let failures = run_tests(&opts, &tests);
    if failures.is_empty() {
        println!(
            "all {}{}{} test(s) passed! {}<3{}",
            color::Fg(color::Green),
            tests.len(),
            color::Fg(color::Reset),
            color::Fg(color::Red),
            color::Fg(color::Reset),
        );
        EXIT_SUCCESS
    } else {
        println!(
            "{}{}{} of {} test(s) failed! {}</3{}",
            color::Fg(color::Red),
            failures.len(),
            color::Fg(color::Reset),
            tests.len(),
            color::Fg(color::Red),
            color::Fg(color::Reset),
        );
        println!("failures:");
        for failure in failures.iter() {
            println!("{}", failure);
        }
        EXIT_FAILURE
    }
}

fn main() {
    let opts = Opts::from_args();
    let exit_code = driver(&opts);
    println!("exiting with status code {}", exit_code);
    process::exit(exit_code);
}
