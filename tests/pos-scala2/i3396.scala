object Test {

  trait Tagged[A]

  // Negation Tagged: NotTagged[A] is available only if there are no Tagged[A] in scope.
  trait NotTagged[A]
  trait NotTaggedLowPrio {
    implicit def notTaggedInstance[A]: NotTagged[A] = null
  }
  object NotTagged extends NotTaggedLowPrio {
    implicit def notTaggedAmbiguity1[A](implicit ev: Tagged[A]): NotTagged[A] = null
    implicit def notTaggedAmbiguity2[A](implicit ev: Tagged[A]): NotTagged[A] = null
  }


  case class Foo[A](value: Boolean)
  trait FooLowPrio {
    implicit def fooDefault[A]: Foo[A] = Foo(true)
  }
  object Foo extends FooLowPrio {
    implicit def fooNotTagged[A](implicit ev: NotTagged[A]): Foo[A] = Foo(false)
  }


  def main(args: Array[String]): Unit = {
    implicit val taggedInt: Tagged[Int] = null

    assert(implicitly[Foo[Int]].value) // fooDefault

    assert(!implicitly[Foo[String]].value) // fooNotTagged

    println(1)
  }
}
