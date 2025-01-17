trait WrapperType[A]

case class Foo[A]()

case class Bar[A]()

type FooToBar[D[_]] = [A] =>> D[Unit] match {
  case Foo[Unit] => Bar[A]
}

case class Test()
object Test {
  implicit val wrapperType: WrapperType[Bar[Test]] = new WrapperType[Bar[Test]] {}
}

val test = summon[WrapperType[FooToBar[Foo][Test]]]
