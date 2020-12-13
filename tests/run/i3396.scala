import scala.util.NotGiven

object Test {

  trait Tagged[A]

  case class Foo[A](value: Boolean)
  trait FooLowPrio {
    implicit def fooDefault[A]: Foo[A] = Foo(true)
  }
  object Foo extends FooLowPrio {
    implicit def fooNotTagged[A](implicit ev: NotGiven[Tagged[A]]): Foo[A] = Foo(false)
  }


  def main(args: Array[String]): Unit = {
    implicit val taggedInt: Tagged[Int] = null

    assert(implicitly[Foo[Int]].value) // fooDefault

    assert(!implicitly[Foo[String]].value) // fooNotTagged

    println(1)
  }
}
