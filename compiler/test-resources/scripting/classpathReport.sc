#!/usr/bin/env -S bin/scala -classpath 'dist/target/pack/lib/*'

import java.nio.file.Paths

def main(args: Array[String]): Unit =
  val cwd = Paths.get(".").toAbsolutePath.normalize.toString.norm
  printf("cwd: %s\n", cwd)
  printf("classpath: %s\n", sys.props("java.class.path").norm)

extension(s: String)
  def norm: String = s.replace('\\', '/')

