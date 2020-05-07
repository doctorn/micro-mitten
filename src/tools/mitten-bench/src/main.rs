use serde::Serialize;

use std::error::Error;
use std::ffi::OsStr;
use std::fs::{self, OpenOptions};
use std::io::{self, Write as _};
use std::path::{Path, PathBuf};
use std::process::{self, Output};
use std::time::{Duration, Instant};
use std::{cmp, env, fmt};

use structopt::StructOpt;

use termion::{clear, color, cursor};

const EXIT_SUCCESS: i32 = 0;
const EXIT_FAILURE: i32 = 1;

const CACHE_PROFILER: &str = "cachegrind";
const HEAP_PROFILER: &str = "massif";

const PROBLEM_SIZE: &str = "MITTEN_PROBLEM_SIZE";
const ITERS: &str = "MITTEN_ITERS";

#[derive(StructOpt, Debug)]
#[structopt(name = "mitten-bench")]
pub struct Opts {
    #[structopt(long, parse(from_os_str), default_value = "mmtnc")]
    pub build: PathBuf,
    #[structopt(long = "path", parse(from_os_str), default_value = "./")]
    pub benchmark_path: PathBuf,
    #[structopt(long, short = "v")]
    pub verbose: bool,
    #[structopt(long = "iterations", short = "i", default_value = "10")]
    pub iterations: u32,
    #[structopt(long = "repeats", short = "r", default_value = "100")]
    pub repeats: u32,
    #[structopt(long = "magnitude", short = "m", default_value = "3")]
    pub magnitude: u32,
    #[structopt(long = "export-csv", parse(from_os_str))]
    pub csv_file: Option<PathBuf>,
    #[structopt(long = "profile-cache", short = "-C")]
    pub profile_cache: bool,
    #[structopt(long = "profile-heap", short = "-H")]
    pub profile_heap: bool,
    #[structopt(short = "-")]
    pub mmtnc_args: Vec<String>,
}

#[derive(Debug, Serialize)]
struct BenchmarkDataPoint {
    #[serde(rename(serialize = "Benchmark"))]
    path: PathBuf,
    #[serde(rename(serialize = "GC Strategy"))]
    gc_strategy: mitten_util::GcStrategy,
    #[serde(rename(serialize = "Compile Time (microseconds)"))]
    compile_time: u128,
    #[serde(rename(serialize = "Binary Size (bytes)"))]
    binary_size: u64,
    #[serde(rename(serialize = "Problem Size"))]
    problem_size: u32,
    #[serde(rename(serialize = "Iterations"))]
    iterations: u32,
    #[serde(rename(serialize = "Total (milliseconds)"))]
    duration_millis: u128,
    #[serde(rename(serialize = "Iteration (microseconds)"))]
    micros_per_iter: u128,
    #[serde(rename(serialize = "Optimised"))]
    optimised: bool,
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
    CSVExport,
    CompileError(Output),
    ClangError(Output),
}

impl fmt::Display for FailureKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FailureKind::Fatal => write!(f, "a fatal error occured"),
            FailureKind::CSVExport => write!(
                f,
                "an error occured while attempting to export results as CSV"
            ),
            FailureKind::CompileError(output) => {
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

fn collect_benchmarks(opts: &Opts) -> Result<Vec<PathBuf>, Failure> {
    let mut benchmarks = vec![];
    let mmtn_extension = OsStr::new(mitten_util::EXT);
    mitten_util::visit_dirs(&opts.benchmark_path, &mut |path| {
        if let Some(extension) = path.extension() {
            if extension == mmtn_extension {
                if opts.verbose {
                    println!("found {}", &path.display());
                }
                benchmarks.push(path.to_owned());
            }
        }
        Ok(())
    })
    .map_err(|err| FailureKind::Fatal.with_path_and_cause(&opts.benchmark_path, Box::new(err)))?;
    println!(
        "found {}{}{} benchmark(s)",
        color::Fg(color::Green),
        benchmarks.len(),
        color::Fg(color::Reset),
    );
    Ok(benchmarks)
}

fn run_benchmark(
    opts: &Opts,
    path: &Path,
    problem_size: u32,
    gc_strategy: mitten_util::GcStrategy,
    optimised: bool,
    compile_time: Duration,
    benchmark: &Path,
) -> Result<Vec<BenchmarkDataPoint>, Failure> {
    let mut series = vec![];
    let start = Instant::now();
    for repeat in 1..=opts.repeats {
        if repeat == 1 {
            print!(
                "{}(problem size {}{}{}; repeat {}{}{}/{}) ",
                cursor::Save,
                color::Fg(color::Magenta),
                problem_size,
                color::Fg(color::Reset),
                color::Fg(color::Cyan),
                repeat,
                color::Fg(color::Reset),
                opts.repeats,
            );
            io::stdout().flush().unwrap();
        }
        let repeat_start = Instant::now();
        // run
        let completed = mitten_util::run(
            path,
            opts.verbose,
            None,
            vec![],
            &|err| FailureKind::Fatal.with_path_and_cause(benchmark, Box::new(err)),
            |output| Ok(output.status.code().is_some()),
        )?;
        if completed {
            let repeat_elapsed = repeat_start.elapsed();
            series.push(BenchmarkDataPoint {
                path: benchmark.to_owned(),
                gc_strategy,
                problem_size,
                iterations: opts.iterations,
                duration_millis: repeat_elapsed.as_millis(),
                micros_per_iter: repeat_elapsed.as_micros() / opts.iterations as u128,
                compile_time: compile_time.as_micros(),
                binary_size: fs::metadata(path)
                    .map_err(|err| {
                        FailureKind::Fatal.with_path_and_cause(benchmark, Box::new(err))
                    })?
                    .len(),
                optimised,
            });
        }
        let total_elapsed = start.elapsed();
        print!(
            "{}{}(problem size {}{}{}; repeat {}{}{}/{} ({} failures); {}{}{}s elpased; ~{}{}{}s remaining) ",
            cursor::Restore,
            clear::UntilNewline,
            color::Fg(color::Magenta),
            problem_size,
            color::Fg(color::Reset),
            color::Fg(color::Cyan),
            repeat,
            color::Fg(color::Reset),
            opts.repeats,
            repeat - series.len() as u32,
            color::Fg(color::Cyan),
            total_elapsed.as_secs(),
            color::Fg(color::Reset),
            color::Fg(color::Cyan),
            ((opts.repeats - repeat) as u64 * total_elapsed.as_secs() / repeat as u64),
            color::Fg(color::Reset),
        );
        io::stdout().flush().unwrap();
    }
    print!("{}{}", cursor::Restore, clear::UntilNewline);
    io::stdout().flush().unwrap();
    if opts.profile_cache {
        mitten_util::run(
            path,
            opts.verbose,
            Some(CACHE_PROFILER),
            vec![&format!(
                "--cachegrind-out-file={}",
                benchmark
                    .with_extension(&format!("{}.{}.out.cachegrind", problem_size, gc_strategy))
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
            )],
            &|err| FailureKind::Fatal.with_path_and_cause(benchmark, Box::new(err)),
            |output| {
                if opts.verbose {
                    println!("{}", String::from_utf8(output.stderr).unwrap());
                }
                Ok(())
            },
        )?;
    }
    if opts.profile_heap {
        mitten_util::run(
            path,
            opts.verbose,
            Some(HEAP_PROFILER),
            vec![
                &format!(
                    "--massif-out-file={}",
                    benchmark
                        .with_extension(&format!("{}.{}.out.massif", problem_size, gc_strategy))
                        .file_name()
                        .unwrap()
                        .to_string_lossy()
                ),
                "--time-unit=i",
                "--alloc-fn=GC_malloc",
                "--pages-as-heap=yes",
            ],
            &|err| FailureKind::Fatal.with_path_and_cause(benchmark, Box::new(err)),
            |output| {
                if opts.verbose {
                    println!("{}", String::from_utf8(output.stderr).unwrap());
                }
                Ok(())
            },
        )?;
    }
    Ok(series)
}

fn build_benchmark(
    opts: &Opts,
    benchmark: &Path,
    problem_size: u32,
    gc_strategy: mitten_util::GcStrategy,
    optimise: bool,
    path: &Path,
) -> Result<Vec<BenchmarkDataPoint>, Failure> {
    let start = Instant::now();
    // compile ll
    mitten_util::ll_to_bin(
        &path,
        opts.verbose,
        gc_strategy,
        optimise,
        &|err| FailureKind::Fatal.with_path_and_cause(benchmark, Box::new(err)),
        |output, bin| {
            let elapsed = start.elapsed();
            if !output.status.success() {
                Err(FailureKind::ClangError(output).with_path(benchmark))
            } else {
                run_benchmark(
                    opts,
                    bin,
                    problem_size,
                    gc_strategy,
                    optimise,
                    elapsed,
                    benchmark,
                )
            }
        },
    )
}

fn compile_benchmark(
    opts: &Opts,
    benchmark: &Path,
    gc_strategy: mitten_util::GcStrategy,
) -> Result<Vec<BenchmarkDataPoint>, Failure> {
    let mut results = vec![];
    let magnitude = cmp::min(cmp::max(opts.magnitude, 1), 6);
    for optimise in &[false, true] {
        for i in 1..=20 {
            let problem_size = i * 10u32.pow(magnitude);
            env::set_var(PROBLEM_SIZE, &format!("{}", problem_size));
            // compile mmtn
            results.append(&mut mitten_util::mmtn_to_ll(
                &opts.build,
                benchmark,
                opts.verbose,
                gc_strategy,
                opts.mmtnc_args.as_slice(),
                &|err| FailureKind::Fatal.with_path_and_cause(benchmark, Box::new(err)),
                |output, ll| {
                    if !output.status.success() {
                        Err(FailureKind::CompileError(output).with_path(benchmark))
                    } else {
                        build_benchmark(opts, benchmark, problem_size, gc_strategy, *optimise, ll)
                    }
                },
            )?);
        }
    }
    Ok(results)
}

fn export_csv(opts: &Opts, results: &Vec<BenchmarkDataPoint>) {
    fn serialize(opts: &Opts, results: &Vec<BenchmarkDataPoint>) -> Result<(), Failure> {
        if let Some(csv_file) = &opts.csv_file {
            print!("exporting CSV to {}... ", csv_file.display());
            let file = OpenOptions::new()
                .write(true)
                .create(true)
                .open(csv_file)
                .map_err(|err| {
                    FailureKind::CSVExport.with_path_and_cause(csv_file, Box::new(err))
                })?;
            let mut writer = csv::Writer::from_writer(file);
            for result in results.iter() {
                writer.serialize(result).map_err(|err| {
                    FailureKind::CSVExport.with_path_and_cause(csv_file, Box::new(err))
                })?;
            }
            writer
                .flush()
                .map_err(|err| FailureKind::CSVExport.with_path_and_cause(csv_file, Box::new(err)))
        } else {
            print!("no CSV export target specified, skipping... ");
            Ok(())
        }
    }

    if !results.is_empty() {
        match serialize(opts, results) {
            Err(failure) => {
                println!("{}FAIL{}", color::Fg(color::Red), color::Fg(color::Reset));
                println!("{}", failure)
            }
            _ => println!("{}DONE{}", color::Fg(color::Green), color::Fg(color::Reset)),
        }
    }
}

fn run_benchmarks(
    opts: &Opts,
    benchmarks: &Vec<PathBuf>,
    results: &mut Vec<BenchmarkDataPoint>,
) -> Vec<Failure> {
    let mut failures = vec![];
    for strategy in &[
        mitten_util::GcStrategy::None,
        mitten_util::GcStrategy::BDW,
        mitten_util::GcStrategy::Proust,
    ] {
        println!(
            "using strategy '{}{}{}'...",
            color::Fg(color::Yellow),
            strategy,
            color::Fg(color::Reset)
        );
        for benchmark in benchmarks.iter() {
            print!("running {}... ", benchmark.display());
            io::stdout().flush().unwrap();
            if opts.verbose {
                println!();
            }
            let compile_start = Instant::now();
            match compile_benchmark(opts, benchmark, *strategy) {
                Ok(series) if opts.repeats != 0 && opts.iterations != 0 => {
                    println!("{}OK{}", color::Fg(color::Green), color::Fg(color::Reset));
                    for data_point in series.into_iter() {
                        results.push(data_point);
                    }
                }
                Ok(_) => {
                    println!("{}OK{}", color::Fg(color::Green), color::Fg(color::Reset));
                }
                Err(failure) => {
                    failures.push(failure);
                    println!(
                        "{}FAIL{} (after {}{}{} ms)",
                        color::Fg(color::Red),
                        color::Fg(color::Reset),
                        color::Fg(color::Cyan),
                        compile_start.elapsed().as_millis(),
                        color::Fg(color::Reset),
                    );
                }
            }
        }
    }
    failures
}

fn driver(opts: &Opts) -> i32 {
    let benchmarks = match collect_benchmarks(&opts) {
        Ok(benchmarks) => benchmarks,
        Err(failure) => {
            println!("{}", failure);
            return EXIT_FAILURE;
        }
    };
    if (opts.repeats == 0 || opts.iterations == 0) && !opts.profile_cache && !opts.profile_heap {
        println!("nothing to do!",);
        return EXIT_SUCCESS;
    } else if opts.iterations != 0 && opts.repeats != 0 {
        env::set_var(ITERS, &format!("{}", opts.iterations));
        println!(
            "running benchmarks {}{}{} time(s) for {}{}{} iteration(s)",
            color::Fg(color::Cyan),
            opts.repeats,
            color::Fg(color::Reset),
            color::Fg(color::Cyan),
            opts.iterations,
            color::Fg(color::Reset)
        );
    }
    let mut results = vec![];
    let failures = run_benchmarks(&opts, &benchmarks, &mut results);
    if opts.verbose {
        println!("{:#?}", results);
    }
    export_csv(opts, &results);
    if failures.is_empty() {
        println!(
            "all {}{}{} benchmarks(s) run successfully! {}<3{}",
            color::Fg(color::Green),
            benchmarks.len(),
            color::Fg(color::Reset),
            color::Fg(color::Red),
            color::Fg(color::Reset),
        );
        EXIT_SUCCESS
    } else {
        println!(
            "{}{}{} of {} benchmarks(s) failed! {}</3{}",
            color::Fg(color::Red),
            failures.len(),
            color::Fg(color::Reset),
            benchmarks.len(),
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
    process::exit(exit_code)
}
