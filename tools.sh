#!/bin/bash

cargo install --path ./src/tools/knit/ --force
cargo install --path ./src/tools/mitten-bench/ --force
cargo install --path ./src/tools/mitten-test/ --force

rustup override set --path ./src/rt/ nightly

cd ./src/rt/
cargo build --release
cd ../../

sudo cp ./src/rt/target/release/libmitten_rt.so /lib/