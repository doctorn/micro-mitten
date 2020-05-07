# `micro-mitten`

metal's too hot? wear a mitten!

[![Build Status](https://travis-ci.com/doctorn/micro-mitten.svg?token=KhJmSEzcFG1aixcpAAvB&branch=master)](https://travis-ci.com/doctorn/micro-mitten)

## `micro-mitten`??

`micro-mitten` is a bare-bones Rust-like programming language, stripped down to simplify control-flow structures and the type system.

Like Rust, `micro-mitten` offers a static approach to memory management; however, `micro-mitten`'s approach is significantly different from Rust's. Rather than depending on single ownership and a complex lifetime system, `micro-mitten` uses a series of data-flow analyses to statically approximate heap liveness. This means that it maintains the ability to insert freeing code at appropriate program points, without putting restrictions on how you write your code. The theory behind the approach is documented in [this thesis](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-908.pdf) (Proust 2017).

Long story short, this is an attempt to see if we really can have unrestrictive compile-time garbage collection.

## How can I use it?!

The project depends on `libgc` and the LLVM-8 toolchain. Cloning this repository and running the following should get you set up with a working copy of `mmtnc` (the `micro-mitten` compiler).

```sh
./tools.sh # installs `mitten-test`, `mitten-bench`, `knit` and the language runtime
cargo install --path ./ --force # installs `mmtnc`
```

`mmtnc` compiles `.mmtn` files to LLVM IR (`.ll`) and just dumps the textual representation. Much more helpfully, you can get binaries directly using `knit`. To build a binary (`example`) from a source file (`example.mmtn`) use:

```sh
knit --src example.mmtn --gc-strategy=proust 
```

The `--gc-strategy` argument is optional, but the default is to use *no* garbage collection (your computer will hate you). The other options are `proust` (uses static memory management) and `bdw` (uses `libgc`).

## Notes

To get an idea of the language syntax, check out the examples in `src/test/`. If you get something wrong, the compiler shoud moan at you. Otherwise, please open an issue!

Please note, `micro-mitten` is purely a research language and its performance is not brilliant when using static memory management. For a full run-down, see [my dissertation](http://nathancorbyn.com/nc513.pdf). Most of the examples in `src/test/` will run with static memory management, but there are some notable exceptions and one or two memory leaks.
