#!/usr/bin/env bash
set -o pipefail
set -e
## set -x
this_dir="$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"

pushd "$this_dir"
  make build
popd

num_tests=${1:-9}

write_a_c_compiler_dir=${WACC_DIR:-../write_a_c_compiler}
cd "$write_a_c_compiler_dir"

./test_compiler.sh "$this_dir/tnqcc" $num_tests
