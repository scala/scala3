#!/bin/bash

run="false"
clean="false"
mainclass="Main"
LIST_ARGS=()

while getopts "rcm:p:" flag
do
    case "${flag}" in
        r) run="true";;
        c) clean="true";;
        m) mainclass=$OPTARG;;
        p) LIST_ARGS+=("$OPTARG")
    esac
done

if [[ "$clean" == "true" ]]; then
  cd tests/valhalla/bin
  rm -rf *
fi

if [[ "$run" == "true" ]]; then
  ./../../Thesis/valhalla/build/macosx-aarch64-server-release/images/jdk/bin/java -cp library/target/scala-library-nonbootstrapped/scala-library-3.8.5-RC1-bin-SNAPSHOT-nonbootstrapped.jar:tests/valhalla/bin --enable-preview "$mainclass" "$LIST_ARGS"
fi