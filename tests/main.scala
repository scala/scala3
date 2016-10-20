

//object Foo {
//  def foo() = System.out.println(42)
//  foo()
//}

object Test {
//  foo.`package`
  def foo() = System.out.println(42)
  foo()

//    scala.`package`


  def main(args: Array[String]): Unit = {
//    Test
//    System.out.println("============")
//    System.out.println("hello")
//    Foo
//
//    scala.math.BigInt(1)

//    barPoly("a", "b")
//    fooPoly(1)
//    scala.BigInt
//    fooInt(1, 2)
//    fooAnyRef("a")
//    List()
//    List(1, 2, 3)
//    List("1")
//    scala.Predef

//    new Bar
//    Vector.empty
//    scala.Vector
//    scala.collection.mutable.Seq()
//    Foo
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

//class Baz extends Comparable[Baz] {
//  def foo: Int = 42
//  override def toString: String = "BazBaz"
//
//  override def compareTo(o: Baz): Int = hashCode() - o.hashCode()
//}
//
//class Bar extends Foo {
////  val bar: Int = 43
//}
//
//trait Foo {
//  val foo: Int = foo
//  def foo0 = 42
//}


//object Bar {
//  val bar: Int = 42
//  def baz: Int = bar
//}
//
//
//object Foo {
//  val foo = Bar.baz
//}

//package foo {
//  package object foo {
//    val foo1 = foo2()
//    def foo2() = System.out.println("hello foo")
//    foo2()
//
//    Baz.baz
//  }
//}
//
//
////object Bar {
////  foo.`package`
////  def a = {
////    foo.toString
////    1
////  }
////}
//
//object Baz {
//  def baz = 42
//}