#!/usr/bin/env bash

COMMAND="./rka-2-dka"

fail() {
    echo "Failed: $1"
    exit 1
}

echo "Invalid argument -X"
${COMMAND} -X && fail "Argument -X is invalid, but the program did not fail"

echo "Using -i"
${COMMAND} -i input/simple.fa || fail "Argument -i and input file is valid, nothing should go wrong"

echo "Using -r"
${COMMAND} -i input/simple.fa || fail "Argument -r and input file is valid, nothing should go wrong"

echo "SUCCESS: All tests have passed"