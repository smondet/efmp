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

if [ "$1" = "--interactive" ] || [ "$1" = "-i" ]; then
  if [ $# -le 1 ] ; then
    echo "Not enough args"
    exit 1
  fi
  persist_on="/tmp/efmp_test_interactive"
  shift
  cmd=$1
  shift 
  ocaml ./src/test/efmp_test.ml $cmd -P $persist_on $*
  code=$?
  echo "Returned $code (persisting on: $persist_on)"
  exit $code
fi

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

echo "======================= TEST WAIT FOR ==========================="
tmp_file=`mktemp`
rm $tmp_file
ocaml ./src/test/efmp_test.ml start -P $persist_on test-wait-for $tmp_file
run_test wake-up

touch $tmp_file
run_test wake-up


sleep=2
echo "======================== SLEEP $sleep =========================="
sleep $sleep
run_test wake-up
run_test wake-up
run_test wake-up

echo "=================================================="
echo "info --all"
echo "=================================================="
ocaml ./src/test/efmp_test.ml info -P $persist_on --all --item-format '* $name: $status$n'

rm -fr $persist_on
