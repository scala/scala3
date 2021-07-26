#!/usr/bin/env scala

// precise output format expected by BashScriptsTests.scala
def main(args: Array[String]): Unit =
  for (a,i) <- args.zipWithIndex do
    printf(s"arg %2d:[%s]\n",i,a)
