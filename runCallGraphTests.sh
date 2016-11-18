#!/bin/sh

source ~/.bash_profile

callSite=`pwd`

function findScalaSources() {
  echo `(ls ../*.scala) 2> /dev/null`
}

function findJavaSources() {
  echo `(ls ../*.java) 2> /dev/null`
}

function findAuxJavaSources() {
  echo `(ls ../java/*.java) 2> /dev/null`
}


function compile() {
  sc_srcs=`findScalaSources`
  jv_srcs=`findJavaSources`
  jv_aux_srcs=`findAuxJavaSources`

  # if there are java sources and a java compilation command is given
  if [ -n "$jv_srcs" ] && [ -n "$jv_cmp_cmd" ]; then
    ( dot_compile -link-dce -link-vis "$sc_srcs $jv_srcs" && $jv_cmp_cmd $jv_srcs ) > compile-log.txt 2> compile-errors.txt || return 1
  else
    dot_compile -link-dce -link-vis "$sc_srcs" > compile-log.txt 2> compile-errors.txt || return 1
  fi

  # if there are auxiliary java sources and a java compilation command is given
  if [ -n "$jv_aux_srcs" ]; then
    javac $jv_aux_srcs -d ./ > compile-log.txt 2> compile-errors.txt || return 1
  fi
}

function run() {
  dottyr Test > run-log.txt 2> run-errors.txt
}

if [ $# -eq 0 ]; then
  test_dirs=tests/callgraph/*
else
  test_dirs=$@
fi

code=0

for test_dir in $test_dirs; do

  echo "Testing" $test_dir
  pushd $test_dir > /dev/null
  rm -R out > /dev/null 2> /dev/null
  mkdir -p out
  if [ -e "env" ]
  then
    source ./env
  fi
  cd out > /dev/null
  compile "$test_dir/out/" && run || { echo "... failed"; code=1; }
  popd > /dev/null

  if [ -e "$test_dir/out/run-log.txt" ]
  then
    if [ -s "$test_dir/out/run-errors.txt" ]
    then
      echo "... failed run";
      cat $test_dir/out/run-errors.txt
    else
      diff -y $test_dir/out/run-log.txt $test_dir/check.txt > $test_dir/out/run-log-diff.txt && echo "... passed" || { echo "... diff failed"; cat $test_dir/out/run-log-diff.txt; code=1; }
    fi
  else
    echo "... failed compilation";
    cat $test_dir/out/compile-errors.txt
  fi

  echo ""
done

if [ $code -eq 0 ]; then
  exit 1
fi
