#!/bin/sh

# Bisect a shell command. Takes a `xargs -I{}` style lambda as argument, finds
# the first value of `{}` < 65536 such that the command succeeds. Usage:
#
# $ sh bisect.sh test {} -lt 42

cmd="$@"

# Example of bisection to isolate -optimise bootstrap errors using -Yopt-fuel.
# See comments in dotty/tools/dotc/transform/localopt/Simplify.scala

# cmd=$(cat <<EOF
#   sbt '
#     ; project dotty-compiler-bootstrapped
#     ; clean
#     ; set scalacOptions ++= Seq(
#       "-optimise",
#       "-Yopt-phases:InlineCaseIntrinsics",
#       "-Yopt-fuel", "{}",
#       "-pagewidth", "1024")
#     ; test
#   '
# EOF
# )

lo=1
hi=65536 # ~16 steps

while [ "$(echo "$hi - $lo" | bc)" -ne 1 ]; do
  mid=$(echo "$lo + ($hi - $lo) / 2" | bc)
  run=$(echo "$cmd" | sed "s/{}/$mid/g")

  echo "Now trying with {} = $mid (lo=$lo, hi=$hi)"
  echo "$run"
  sh -c "$run"

  if [ $? -eq 0 ]; then lo=$mid; else hi=$mid; fi
done
echo "First failure at $hi"
