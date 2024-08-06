
trait Foo[-T]
trait Bar[-T] extends Foo[T]

object Test {

  locally:
    implicit val fa: Foo[Int] = ???
    implicit val ba: Bar[Int] = ???
    summon[Foo[Int]] // ok

  locally:
    implicit val fa: Foo[Int] = ???
    implicit val ba: Bar[Any] = ???
    summon[Foo[Int]] // ok

  locally:
    given fa: Foo[Any] = ???
    given ba: Bar[Int] = ???
    summon[Foo[Int]] // error: now ambiguous,
    // was resolving to `ba` when using intermediate rules:
    // better means specialize, but map all type arguments downwards

  locally:
    implicit val fa: Foo[Any] = ???
    implicit val ba: Bar[Int] = ???
    summon[Foo[Int]] // is OK since we fall back to old rules for old-style implicits as a tie breaker
}
