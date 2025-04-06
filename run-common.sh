#!/bin/bash

help() {
    echo "Usage: $0 [commands] <argument>"
    echo "commands:"
    echo "  test <argument> : Run all tests."
    echo "  doc             : Run doc test."
    echo "  exam <argument> : Run all examples."
    echo "  all <argument>  : Run all tests and examples."
    echo "  clean           : Clean project."
    echo "arguments:"
    echo "  -r    : Release mode."
    echo "  -a    : Debug & Release modes."
    echo "  -R    : Run recursively."
    exit 1
}

print_title() {
    local crate_name=$(get_crate_name)
    local input_string="$crate_name: $1"
    local line_length=80
    local border=$(printf '%*s' "$line_length" | tr ' ' '=')
    local padding=$(( (line_length - ${#input_string} - 2) / 2 ))
    local left_padding=$(printf '%*s' "$padding" '')
    local right_padding=$(printf '%*s' "$((line_length - ${#input_string} - 2 - padding))" '')

    echo "$border"
    echo "=$left_padding$input_string$right_padding="
    echo "$border"
}

get_host_triple() {
    rustc -vV | grep host | cut -d ' ' -f2
}

get_crate_name() {
    grep '^name = "' Cargo.toml | sed -E 's|.*"([^/]+)"|\1|' | head -n 1
}

run_crates() {
    local crates=($(ls -d crates/*/ 2> /dev/null))
    local ret=0

    for crate in ${crates[@]}; do
        pushd . > /dev/null
        cd $crate
        if [ -f "run.sh" ]; then
            ./run.sh $cmd $opt_args
            ret=$?
            if [ $ret -ne 0 ]; then
                popd > /dev/null
                exit $ret
            fi
        fi
        popd > /dev/null
    done
}

test_debug() {
    local ret=0

    if [ $is_debug -eq 1 ]; then
        print_title "Test on Debug build"
        cargo test --tests --target $(get_host_triple)
        ret=$?
        if [ $ret -ne 0 ]; then
            exit $ret
        fi
    fi
}

test_release() {
    local ret=0

    if [ $is_release -eq 1 ]; then
        print_title "Test on Release build"
        cargo test --tests -r --target $(get_host_triple)
        ret=$?
        if [ $ret -ne 0 ]; then
            exit $ret
        fi
    fi
}

test_doc() {
    local ret=0

    print_title "Doc Test"
    cargo test --doc --target $(get_host_triple)
    ret=$?
    if [ $ret -ne 0 ]; then
        exit $ret
    fi
}

run_examples() {
    local files=$(grep '^path = "examples/' Cargo.toml | sed -E 's|.*/([^/]+)\.rs"|\1|')
    local names=(${files})
    local ret=0

    if [ $is_debug -eq 1 ]; then
        for name in "${names[@]}"; do
            print_title "Example $name on Debug build"
            cargo run --example $name --target $(get_host_triple)
            ret=$?
            if [ $ret -ne 0 ]; then
                exit $ret
            fi
        done
    fi
    if [ $is_release -eq 1 ]; then
        for name in "${names[@]}"; do
            print_title "Example $name on Release build"
            cargo run --example $name -r --target $(get_host_triple)
            ret=$?
            if [ $ret -ne 0 ]; then
                exit $ret
            fi
        done
    fi
}

clean() {
    print_title "Clean Lib"
    cargo clean
    rm Cargo.lock
}

is_debug=1
is_release=0
test_kind=""
is_recursive=0
all_args=("$@")
opt_args=${all_args[@]:1}

for arg in $opt_args
do
    case $arg in
        -r)
            is_debug=0
            is_release=1
            ;;
        -a)
            is_debug=1
            is_release=1
            ;;
        -tsan)
            is_debug=0
            is_release=0
            test_kind="tsan"
            ;;
        -rep)
            is_debug=0
            is_release=0
            test_kind="rep"
            ;;
        -R)
            is_recursive=1
            ;;
        *)
            echo "Invalid argument: $arg"
            help
            ;;
    esac
done

cmd=${all_args[0]}

case $cmd in
    test)
        test_debug
        test_release
        ;;
    doc)
        test_doc
        ;;
    exam)
        run_examples
        ;;
    all)
        test_doc
        test_debug
        test_release
        run_examples
        ;;
    clean)
        clean
        ;;
    *)
        echo "Invalid command: $cmd"
        help
        ;;
esac

if [ $is_recursive -eq 1 ]; then
    run_crates
fi
