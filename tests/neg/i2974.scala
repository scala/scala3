
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
    summon[Foo[Int]] // error: ambiguous
}
