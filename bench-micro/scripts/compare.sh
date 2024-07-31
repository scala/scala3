#!/usr/bin/env bash

name=$1
commit_1=$2
commit_2=$3
runs=2

if [ -z "$name" ] || [ -z "$commit_1" ] || [ -z "$commit_2" ]; then
    echo "Usage: $0 <benchmark-name> <commit-1> <commit-2>"
    exit 1
fi

run_benchmark() {
    jvm=$1
    jvm_coursier_id=$2
    run=$3
    hash=$4
    json_file=../../results/$hash-$name-$jvm-$run.json
    txt_file=../../results/$hash-$name-$jvm-$run.txt
    echo "Running $name benchmark with JVM $jvm at commit $hash, run $run out of $runs"
    git checkout $hash
    git clean -fdx
    eval "$(coursier java --jvm "$jvm_coursier_id" --env)"
    rm -rf "$json_file" "$stdout_file" .bloop .sbt .bsp .metals target
    sbt "clean; scala3-bench-micro / Jmh / run -rf JSON -rff $json_file -o $txt_file $name"
}

read -p "WARNING: This script will brutally reset your Dotty repo (git clean -fdx). Type 'y' to continue." -n 1 -r
echo    # new line
if [[ ! $REPLY =~ ^[Yy]$ ]]; then exit 1; fi

read -p "This script should be run form the Dotty root repository. Type 'y' to continue." -n 1 -r
echo    # new line
if [[ ! $REPLY =~ ^[Yy]$ ]]; then exit 1; fi

read -p "This script will create an empty directory <dotty>/../results to store results. Type 'y' to continue." -n 1 -r
echo    # new line
if [[ ! $REPLY =~ ^[Yy]$ ]]; then exit 1; fi

mkdir ../results

for run in $(seq 1 $runs); do
    for hash in $commit_1 $commit_2; do
        run_benchmark openjdk "adoptium:1.17.0.11" $run $hash
        run_benchmark graal "graalvm-java17:22.3.3" $run $hash
    done
done
