import scala.annotation.tailrec

sealed trait Foo[+A]
case class Bar[A](f: Foo[A]) extends Foo[A]
case class Done[A](a: A) extends Foo[A]

object Test {
  @tailrec
  def runT[A](c: Foo[A]): A = c match {
    case Bar(f) => runT(f)
    case Done(a) => a
  }
}
