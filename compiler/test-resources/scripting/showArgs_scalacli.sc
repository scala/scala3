#!/usr/bin/env bin/scala

// This file is a Scala CLI script.

// precise output format expected by BashScriptsTests.scala
for (a,i) <- args.zipWithIndex do
  printf(s"arg %2d:[%s]\n",i,a)
