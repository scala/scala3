#!/usr/bin/env bin/scala

// def main(args: Array[String]): Unit = { MIGRATION: Scala CLI expects `*.sc` files to be straight-line code
  println(new java.sql.Date(100L))
  System.err.println("SCALA_OPTS="+Option(System.getenv("SCALA_OPTS")).getOrElse(""))
// }
