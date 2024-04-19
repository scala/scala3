#!/usr/bin/env bash

name=$1
runs=2

run_benchmark() {
    jvm=$1
    jvm_coursier_id=$2
    run=$3
    json_file=results/$name-$jvm-$run.json
    txt_file=results/$name-$jvm-$run.txt

    eval "$(coursier java --jvm "$jvm_coursier_id" --env)"
    rm -rf "$json_file" "$stdout_file" .bloop .sbt .bsp .metals target
    sbt "clean; scala3-bench-micro / Jmh / run -rf JSON -rff $json_file -o $txt_file $name"
}

for run in $(seq 1 $runs); do
    run_benchmark openjdk "adoptium:1.17.0.11" $run
    run_benchmark graal "graalvm-java17:22.3.3" $run
done
