sealed trait Parent

trait Wrapper {

  case class Foo(x: Int, y: Int, s: String) extends Parent
  case class Bar(x: Int, y: Int) extends Parent

  println(summon[deriving.Mirror.Of[Parent]]) // error
}
