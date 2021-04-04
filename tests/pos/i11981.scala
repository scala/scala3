object Main:
    class Null
    type Optional[A] = A | Null

    val maybeInt: Optional[Int] = 1

    // simplest typeclass
    trait TC[F[_]]

    // given instances for our Optional and standard Option[_]
    given g1: TC[Optional] = ???
    given g2: TC[Option] = ???

    def summonTC[F[_], A](f: F[A])(using TC[F]): Unit = ???

    summonTC(Option(42))  // OK

    summonTC[Optional, Int](maybeInt) // OK

    summonTC(maybeInt)

