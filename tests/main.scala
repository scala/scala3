import java.util

import scala.util.control.Breaks
import scala.util.control.ControlThrowable
import scala.util.control.NoStackTrace

object Test {
  def main(args: Array[String]): Unit = {
//    System.out.println("============")
//    System.out.println("hello")

//    barPoly("a", "b")
//    fooPoly(1)
//    scala.BigInt
//    fooInt(1, 2)
//    fooAnyRef("a")
//    List()
//    List(1, 2, 3)
//    List("1")

    Predef
//    new Bar
//    Vector.empty
//    scala.Vector
//    scala.collection.mutable.Seq()


//    (scala.collection.immutable.Vector.empty[Int] :+ 1).head
//    val set = new util.HashSet[Baz]()

//    System.out.println("abc")
//    System.out.println(new Baz)
//
//    System.out.println(new Baz().foo)
//new Exception
//    System.out.println(sys.SystemProperties.noTraceSupression.value)
//Predef.println("foo")
//    System.out.println("bye")
  }


//  def barPoly[S](xs: S*) = println("barPoly")
//  def fooPoly[T](x: T) = barPoly(x)
//  def fooInt(xs: Int*) = System.out.println("fooInt")
//  def fooAnyRef(xs: AnyRef*) = System.out.println("fooAnyRef")
}

class Baz extends Comparable[Baz] {
  def foo: Int = 42
  override def toString: String = "BazBaz"

  override def compareTo(o: Baz): Int = hashCode() - o.hashCode()
}

class Bar extends Foo {
//  val bar: Int = 43
}

trait Foo {
  val foo: Int = 42
}