import scala.quoted._
import scala.quoted.autolift._

object Foo {
  inline def foo(): Int = ${bar(${x})} // error
  def x: Staged[Int] = '{1}
  def bar(i: Int): Staged[Int] = i
}
