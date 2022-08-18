# Usage
# > git bisect start
# > git bisect bad <bad-commit>
# > git bisect good <good-commit>
# > git bisect run project/scripts/dottyCompileBisect.sh <file.scala>
#
# Note: Use dottyCompileBisect.scala for faster bisection over commits that spans several days

files=$@
shift

rm -r out
mkdir out
mkdir out/bisect

sbt "clean; scalac -d out/bisect $files"
