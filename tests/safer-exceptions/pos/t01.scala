package test.saferExceptions.pos

import language.experimental.{saferExceptions}
import java.io.IOException
import java.io.FileNotFoundException

class GenericExc[T] extends Exception

def test throws IOException, FileNotFoundException :Unit = throw new IOException("")

// Main method cannot have implicit parameters nor return typpe
// Why ? and how can we avoid this ?
//@main def a(args : Array[String]) throws IOException : Unit =
//  test

trait AFoo :
  def foo(x : Int) throws Exception : Int

class Foo extends AFoo :
  def foo(x : Int) throws IOException, FileNotFoundException : Int = x
