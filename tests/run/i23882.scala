// scalajs: --skip

import scala.collection.*
import scala.caps.Pure

trait Bar

class Foo:
  def foo[T](x: Seq[T] | Set[T]): Unit = ??? // erases to a weird thing: scala.Equals
  def foo[T](x: Array[T]): Unit = ???        // erases to Object
  def bar(x: Bar & Pure): Unit = ???      // erases to Bar
  def bar2(x: Pure & Bar): Unit = ???      // erases to Bar
  def bar3[T <: Pure](x: Bar & T): Unit = ???      // erases to Bar
  def bar4[T <: Pure](x: T & Bar): Unit = ???      // erases to Bar

@main def Test =
  for mtd <- classOf[Foo].getDeclaredMethods do
    println(mtd)
