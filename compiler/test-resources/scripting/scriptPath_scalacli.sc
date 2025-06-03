#!/usr/bin/env bin/scala

// THIS FILE IS RAN WITH SCALA CLI, which wraps scripts exposing scriptPath and args variables

args.zipWithIndex.foreach { case (arg,i) => printf("arg %d: [%s]\n",i,arg) }

if !scriptPath.endsWith("scriptPath_scalacli.sc") then
  printf( s"incorrect script.path defined as [$scriptPath]")
else
  printf("scriptPath: %s\n", scriptPath) // report the value

extension(s: String)
  def norm: String = s.replace('\\', '/')
