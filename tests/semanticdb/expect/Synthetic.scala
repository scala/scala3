package example

import scala.language.implicitConversions

class Synthetic {
  List(1).map(_ + 2)
  Array.empty[Int].headOption
  "fooo".stripPrefix("o")

  // See https://github.com/scalameta/scalameta/issues/977
  val Name = "name:(.*)".r
  val x #:: xs = LazyList(1, 2): @unchecked
  val Name(name) = "name:foo": @unchecked
  1 #:: 2 #:: LazyList.empty

  val a1 #:: a2 #:: as = LazyList(1, 2): @unchecked

  val lst = 1 #:: 2 #:: LazyList.empty

  for (x <- 1 to 10; y <- 0 until 10) println(x -> x)
  for (i <- 1 to 10; j <- 0 until 10) yield (i, j)
  for (i <- 1 to 10; j <- 0 until 10 if i % 2 == 0) yield (i, j)

  object s {
    def apply() = 2
    s()
    s.apply()
    case class Bar()
    Bar()
    null.asInstanceOf[Int => Int](2)
  }

  class J[T: Manifest] { val arr = Array.empty[T] }

  class F
  implicit val ordering: Ordering[F] = ???
  val f: Ordered[F] = new F

  import scala.concurrent.ExecutionContext.Implicits.global
  for {
    a <- scala.concurrent.Future.successful(1)
    b <- scala.concurrent.Future.successful(2)
  } println(a)
  for {
    a <- scala.concurrent.Future.successful(1)
    b <- scala.concurrent.Future.successful(2)
    if a < b
  } yield a

  object Contexts {
    def foo(x: Int)(using Int) = ???
    def m1(using Int) = foo(0)
    def m2(using x: Int) = foo(0)
    def m3 =
      given x: Int = 1
      foo(x)
    def m4 =
      given Int = 1
      foo(0)
  }
}
