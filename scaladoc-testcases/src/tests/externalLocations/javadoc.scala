package tests.externalJavadoc

import java.util.*

class Test {
  def a: Map.Entry[String, String] = ???

  def b: java.util.Map[String, Int] = ???

  def c: java.util.stream.Stream.Builder[String] = ???
}

class MyException extends java.lang.Exception

class MyArrayList[T] extends java.util.ArrayList[T]

trait MyPrintStream extends java.io.PrintStream

