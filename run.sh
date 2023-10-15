#!/bin/bash

my_exit() {
    echo "Usage: $0 command"
    echo "command:"
    echo "  build: Build in debug mode."
    echo "  test: Test all."
    echo "  clean: Remove derectories generated by build process."
    echo "  help: Print this."
    exit 1
}

if [ "$#" -ne 1 ]; then
    my_exit 1
fi

cleandirs=(
    pkg
    target
)

if [ "$1" = "build" ]; then
    wasm-pack build --dev
elif [ "$1" = "test" ]; then
    wasm-pack test --chrome --headless --workspace
elif [ "$1" = "clean" ]; then
    # Direct cleaning
    echo "=== Cleaning root directory... ==="
    for dir in "${cleandirs[@]}"; do
        rm -rf $dir
    done

    # Make npm clean itself
    for dir in "examples"/*; do
        if [ -d "$dir" ] && [ -e "$dir/package.json" ]; then
            echo "=== Cleaning $dir... ==="
            pushd .
            cd $dir
            npm run clean-all
            popd
        fi
    done
    echo "=== Done. ==="
else
    my_exit 1
fi
