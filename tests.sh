#!/usr/bin/env bash

COMMAND="./rka-2-dka"

fail() {
    echo "Should fail, because: $1"
    exit 1
}

echo "Invalid argument -X"
${COMMAND} -X && fail "Invalid argument -X"