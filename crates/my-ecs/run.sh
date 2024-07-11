#!/bin/bash

my_exit() {
    echo "Usage: $0 command"
    echo "command:"
    echo "  test: Tests the crate for the host target"
    echo "  example <name>: Runs the example for the host target"
    exit 1
}

if [ "$1" = "test" ] && [ "$#" -eq 1 ]; then
    rustc -vV | sed -n 's/host: \(\S*\)/\1/p' | xargs -I {} cargo test --target {}
elif [ "$1" = "example" ] && [ "$#" -eq 2 ]; then
    rustc -vV | sed -n 's/host: \(\S*\)/\1/p' | xargs -I {} cargo run --target {} --example $2
else
    my_exit
fi
