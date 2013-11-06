#!/bin/sh

help_message=<<EOF

This script runs \`./src/test/efmp_test.ml\` with various arguments

The arguments are passed to the \`start\` command (useful for setting options)
EOF

persist_on=`mktemp`

run_test() {
  local cmd=$1
  shift 
  ocaml ./src/test/efmp_test.ml $cmd -P $persist_on $*
}

show_info() {
  echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
  echo "$*"
  echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
  run_test info
  echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
  echo ""
}

run_test init
run_test start test
run_test wake-up
show_info After first wake-up

if [ $# -gt 0 ]; then
  run_test start test $*
  run_test wake-up
  show_info After second wake-up
fi

sleep=2
echo "======================== SLEEP $sleep =========================="
sleep $sleep
run_test wake-up
run_test wake-up

echo "=================================================="
echo "info --all"
echo "=================================================="
run_test info --all

rm -fr $persist_on
