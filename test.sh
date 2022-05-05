#!/bin/bash
set -euo pipefail

# build code
pushd code/
nix-build
popd

# copy binaries to another tree
rm -rf build_output/
mkdir -p build_output/bin
cp -v code/result/bin/* build_output/bin/

# log-view switches personality based on argv[0]:
ln -sv log-view build_output/bin/strace-import

# run tests
pushd build_output
bin/test-common
tail --retry -f strace-import.log &
bin/test-log-view
popd

# start strace-import asynchronously
rm -rf strace_logs/
mkdir strace_logs
build_output/bin/strace-import strace_logs &
import_pid=$!
trap "kill $import_pid" EXIT

# calculate shasums for hs files to generate some strace logs
strace -f -tt -o strace_logs/output -ff \
  find . -name '*.hs' -exec bash -c "sha256sum {}; sleep 0.1" \;
kill $import_pid
sleep 5

# start log-view to examine imported strace logs
(
  sleep 1
  echo -en "\n\e[36mopen this link in a browser: "
  echo -e "\e[1mhttp://localhost:8083/strace\e[0m\n"
) &
build_output/bin/log-view strace_logs
