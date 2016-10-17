#!/bin/sh

function dotcf() {
cat /Users/nicolasstucki/GitHub/dotty/test/dotc/scala-collections.whitelist|grep -v '#'| grep -v "^$"| sed 's/.\//\/Users\/nicolasstucki\/GitHub\/dotty\//' | xargs dotc -language:Scala2 $@
}

alias dottyr='java -cp .:/Users/nicolasstucki/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.5.jar:/Users/nicolasstucki/GitHub/dotty/target/scala-2.11/dotty_2.11-0.1-SNAPSHOT.jar'

if [ $# -eq 0 ]
then
    FILES=tests/callgraph/*
else
    FILES=$@
fi

CODE=0

for f in $FILES
do
  echo $f
  pushd $f > /dev/null
  rm -R out > /dev/null 2> /dev/null
  mkdir -p out
  cd out > /dev/null
  dotcf ../Test.scala > compile-log.txt 2> compile-errors.txt && dottyr Test > run-log.txt 2> run-errors.txt || { echo "... failed"; CODE=1;  }
  popd > /dev/null
  diff -y $f/out/run-log.txt $f/check.txt > $f/out/run-log-diff.txt || { echo "... diff failed"; cat $f/out/run-log-diff.txt; CODE=1; }
done

if [ $CODE -eq 0 ]
then
    exit 1;
fi
