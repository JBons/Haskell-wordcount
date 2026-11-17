#!/bin/sh
# Run from any directory; outputs are kept outside the checkout.
set -eu
cd "$(dirname "$0")/.."
test_dir=$(mktemp -d "${TMPDIR:-/tmp}/hwc-counter-tests.XXXXXX")
trap 'rm -rf "$test_dir"' EXIT HUP INT TERM
"${CC:-clang}" -std=c11 -Wall -Wextra -Werror -O1 -g \
  -fsanitize=address,undefined -fno-omit-frame-pointer -pthread \
  -DMAXTRIESIZE=128 -DMAXUNIQUEWORDS=8 -Iinclude \
  test/counterlib_test.c -o "$test_dir/counterlib_test"
"$test_dir/counterlib_test"
