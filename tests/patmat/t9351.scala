// scalac: -Ycheck-all-patmat
trait A {}
case object B extends A {}
case object C extends A {}

class X {
  def good = {
    val a: A = B
    a match {
      case B =>
      case C =>
    }
  }

  def bad = {
    val a: A = B
    val o: Option[Int] = None
    (a, o) match {
      case (B, None) =>
      case (B, Some(_)) =>
      case (C, None) =>
      case (C, Some(_)) =>
    }
  }

  def alsoGood = {
    val a: A = B
    val b: A = C
    (a, b) match {
      case (B, B) =>
      case (B, C) =>
      case (C, B) =>
      case (C, C) =>
    }
  }
}
