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

def printGenericSignature(m: java.lang.reflect.Method): Unit =
  val tpe = m.getParameterTypes().map(_.getTypeName).mkString(", ")
  val ret = m.getReturnType().getTypeName
  println(s"${m.getName}($tpe): $ret")

@main def Test =
  val cls = classOf[Foo]
  printGenericSignature(cls.getDeclaredMethod("foo", classOf[scala.Equals]))
  printGenericSignature(cls.getDeclaredMethod("foo", classOf[Object]))
  printGenericSignature(cls.getDeclaredMethod("bar", classOf[Bar]))
  printGenericSignature(cls.getDeclaredMethod("bar2", classOf[Bar]))
  printGenericSignature(cls.getDeclaredMethod("bar3", classOf[Bar]))
  printGenericSignature(cls.getDeclaredMethod("bar4", classOf[Bar]))
