package tests
package externalScaladoc3

import scala.util.matching.*
import externalStubs._

class Test {
  def a: String = ???

  def b: Map[String, Int] = ???

  def c: Regex.Match = ???
}

class Test2 extends \/ with /\


/** Links to an object of another library, by its first lookup: [[scala.jdk.StreamConverters]] */
class Test3
