
import annotation.tailrec

object Test {
  @tailrec private def quux(xs: List[String]): List[String] =
    quux(
      quux(xs) // error: not in tail position
    )
  @tailrec private def quux2(xs: List[String]): List[String] = xs match {
    case x1 :: x2 :: rest => quux2(
      x1 :: quux2(rest)) // error: not in tail position
    case _ => Nil
  }
  @tailrec private def quux3(xs: List[String]): Boolean = xs match {
    case x :: xs if quux3(List("abc")) => // error: not in tail position
      quux3(xs)
    case _ => false
  }
}
