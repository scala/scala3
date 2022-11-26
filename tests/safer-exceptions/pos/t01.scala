package test.saferExceptions.pos

import language.experimental.saferExceptions
import java.io.IOException
import java.io.FileInputStream
import java.io.FileNotFoundException

class ScalaException extends Exception

def test() throws ScalaException :Unit = throw new ScalaException
/*
-- Warning: tests\safer-exceptions\pos\t01.scala:8:29 --------------------------
8 |class ScalaException extends Exception
  |                             ^^^^^^^^^
  | A Java function was called (<init>) in a context where safer exceptions is enabled.
  | This function might throw an exception.
  | Handling of Java method is yet to be implemented.
*/

// Main method cannot have implicit parameters nor return typpe
// Why ? and how can we avoid this ?
//@main def a(args : Array[String]) throws IOException : Unit =
//  test

trait AFoo :
  def foo(x : Int) throws Exception : Int

class Foo extends AFoo :
  def foo(x : Int) throws IOException, FileNotFoundException : Int = x
