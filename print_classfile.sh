#!/bin/bash

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <class file name> <optional: output file>"
    exit 1
fi

out="/dev/stdout"

if [ "$#" -eq 2 ]; then
    out="$2"
fi

./../../Thesis/valhalla/build/macosx-aarch64-server-release/images/jdk/bin/javap -c -v -p tests/valhalla/bin/"$1" > "$out"
