#!/usr/bin/env bin/scala

// precise output format expected by BashScriptsTests.scala
// MIGRATION: Scala CLI expects `*.sc` files to be straight-line code
for (a,i) <- args.zipWithIndex do
  printf(s"arg %2d:[%s]\n",i,a)
