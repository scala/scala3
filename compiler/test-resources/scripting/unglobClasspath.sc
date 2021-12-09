#!/usr/bin/env -S bin/scala -classpath 'dist/target/pack/lib/*'

// won't compile unless the hashbang line sets classpath
import org.jline.terminal.Terminal

def main(args: Array[String]) =
  val cp = sys.props("java.class.path")
  printf("unglobbed classpath: %s\n", cp)
