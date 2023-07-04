class Ace
class Bar(last: Ace) { implicit val now: Ace = last }
class Foo(now: Ace) extends Bar(now)
class Test {
def test(fst: Foo) = {
  import fst._
  now != null
}
}
