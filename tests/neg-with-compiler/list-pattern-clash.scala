
object test:
  val xs: dotty.tools.List[Int] = ???
  xs match
    case y :: ys => ???  // error: bad pattern match
    case _ => ???
  xs match
    case List(y) => ???  // error: bad pattern match
    case _ => ???

object test2:
  import dotty.tools._
  val xs = scala.List(1)
  xs match
    case y :: ys => ???  // error: bad pattern match
    case _ => ???
  xs match
    case List(y) => ???  // error: bad pattern match
    case _ => ???

