#!/bin/bash

my_exit() {
    echo "Usage: $0 clean"
    exit 1
}

if [ "$#" -ne 1 ]; then
    my_exit 1
fi

cleandirs=(
    pkg
    target
)

if [ "$1" = "clean" ]; then
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
