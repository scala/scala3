package scala.tasty.interpreter.abstr

class Lazy(thunk: => Any) extends Ref {
  lazy val get: Any = thunk
}
