

import language.`3.0-migration`
import scala.language.implicitConversions

object Test1 {
  implicit def implicitLength(s: String): Int = s.length
  val length: Int = "qwerty" // ok
}

object Test2 {
  implicit val implicitLength: Conversion[String, Int] = _.length
  val length: Int = "qwerty" // ok
}

object Test3 {
  implicit val implicitLength: String => Int = _.length
  val length: Int = "qwerty" // warn
}

object Test4 {
  implicit def implicitLength: String => Int = _.length
  val length: Int = "qwerty" // warn
}

object Test5 {
  implicit def implicitLength[A]: String => Int = _.length
  val length: Int = "qwerty" // warn
}

object Test6 {
  implicit val implicitLength: Map[String, Int] = Map("qwerty" -> 6)
  val length: Int = "qwerty" // warn
}

object Test7 {
  implicit def a2int[A](a: A)(implicit ev: A => Int): Int = a // warn
}

object Test8 {
  implicit def a2int[A](a: A)(implicit ev: A => Int): Int = ev(a) // ok
}

object Test9 {
  implicit def a2int[A](a: A)(implicit ev: A <:< Int): Int = a // ok
}

object Test10 {
  trait Foo {
    def foo = "foo"
  }
  implicit def a2foo[A](a: A): Foo = new Foo {}
  123.foo // ok
}

object Test11 {
  trait Foo {
    def foo = "foo"
  }
  implicit def a2foo[A]: A => Foo = _ => new Foo {}
  123.foo // warn
}

object Test12 {
  implicit class FooOps(a: Any) {
    def foo = "foo"
  }
  123.foo // ok
}

object Test13 {
  def foo()(implicit x: String => Int) = ???
  implicit val f: String => Int = _.size
  foo()  // ok
}

object Test14 {
  case class MySeq[A](underlying: Seq[A])

  implicit def mySeq2seq[A](mySeq: MySeq[A]): Seq[A] = mySeq.underlying
  val s: Seq[Int] = MySeq(Seq(1, 2, 3)) // ok
}

object Test15 {
  implicit def implicitSeq[A]: Seq[A] = ???
  def foo(implicit ev: Seq[Int]): Unit = ???
  foo
}