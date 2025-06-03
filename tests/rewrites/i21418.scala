trait Effect[F[_]]
class Countdown[F[_]: Effect]
class Countdown1[F[_]: Effect](howMany: Int)
class Countdown2[F[_]: Effect, F2[_]: Effect]

def foo[F[_]: Effect]() =
  "foo"

@main def Test = {
  val a = new Countdown[Option]()(???)
  Countdown[Option]()(???)
  val b = Countdown[Option]()(???)
  new Countdown[Option] ()(???)
  val c = Countdown[List] () (???)
  new Countdown2[List, Option] ()   (???, ???)
  new Countdown2[List, Option]    (using ???, ???)
  Countdown2[List, Option] ()   (???, ???)
  Countdown2[List, Option] (using ???, ???)
  new Countdown1[Option](10)(???)
  new Array[Int](10)
  new scala.collection.immutable.HashSet[Int]
  new scala.collection.immutable.HashSet[Int]()
  new scala.collection.immutable.HashSet[Int] ()
  foo()(???)
  foo() (???)
}
