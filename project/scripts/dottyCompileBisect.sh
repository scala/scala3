# Usage
# > git bisect start
# > git bisect bad <bad-commit>
# > git bisect good <good-commit>
# > git bisect run project/scripts/dottyCompileBisect.sh [--run <main.class.name>] [<compiler-option> ...] <file1.scala> [<fileN.scala> ...]
#
# Note: Use dottyCompileBisect.scala for faster bisection over commits that spans several days

if [ "$1" == "--run" ]; then
  mainClass="$2"
  shift; shift
fi

compilerArgs=$@

rm -r out
mkdir out
mkdir out/bisect

if [ -n "$mainClass" ]; then
  sbtRunCommand="scala -classpath out/bisect $mainClass"
fi

sbt "clean; scalac -d out/bisect $compilerArgs; $sbtRunCommand"
