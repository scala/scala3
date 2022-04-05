
object Test {
  case class K(i: Int)

  def main(args: Array[String]) = {
    val k = new K(9)
    val is = List(1,2,3)

    is match {
      case List(1, _*,) => // error: pattern expected

      case List(1, _*3,) => // error: pattern expected // error
      case List(1, _*3:) =>  // error // error
      case List(1, x*) => // ok
      case List(x*, 1) => // error: pattern expected
      case (1, x*) => //ok
      case (1, x: _*) => // error: bad use of _* (sequence pattern not allowed)
    }

// good syntax, bad semantics, detected by typer
//gowild.scala:14: error: star patterns must correspond with varargs parameters
    val K(x @ _*) = k
    val K(ns @ _*, xx) = k // error: pattern expected // error
    val K(x) = k // error: x is already defined as value x
    val (b, _ * ) = (5,6) // ok
// no longer complains
//bad-and-wild.scala:15: error: ')' expected but '}' found.
  }
}

