#!/usr/bin/env bin/scala
// This file is a Scala CLI script.
import java.nio.file.Paths

// def main(args: Array[String]): Unit = // MIGRATION: Scala CLI expects `*.sc` files to be straight-line code
  val cwd = Paths.get(".").toAbsolutePath.normalize.toString.norm
  printf("cwd: %s\n", cwd)
  printf("classpath: %s\n", sys.props("java.class.path").norm)

extension(s: String)
  def norm: String = s.replace('\\', '/')

