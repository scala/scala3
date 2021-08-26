
sealed trait Parent

trait Wrapper {

  case class Foo(x: Int, y: Int, s: String) extends Parent
  case class Bar(x: Int, y: Int) extends Parent

  println(summon[deriving.Mirror.Of[Parent]]) // error
}

class ClassWrapper {
  sealed trait Base
  case class Foo(x: Int) extends Base
}

@main def Test =
  val cw = new ClassWrapper()
  val mirrorParent = summon[deriving.Mirror.Of[cw.Base]] // ok
