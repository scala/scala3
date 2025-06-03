type Accumulator[A]

object Accumulator {

  val usage =
    use[Int]:
      "asd"

  inline def use[A](using DummyImplicit): [B] => Any => Any = ???

  inline def use[A]: [B] => Any => Any = ???
}
