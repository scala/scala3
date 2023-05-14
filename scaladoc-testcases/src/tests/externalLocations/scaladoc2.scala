package tests
package externalScaladoc2

import scala.util.matching.*
import externalStubs._

class Test {
  def a: String = ???

  def b: Map[String, Int] = ???

  def c: Regex.Match = ???
}

class Test2 extends \/ with /\

abstract class MySeq[T] extends scala.collection.immutable.Seq[T]

