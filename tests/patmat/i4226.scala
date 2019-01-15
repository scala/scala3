sealed abstract class Maybe[A]
final case class Just[A](a: A) extends Maybe[A]
class Empty[A] extends Maybe[A]
object Empty {
  def apply[A](): Maybe[A] = new Empty[A]
  def unapply[A](e: Empty[A]): true = true
}

object Test {
  val a: Maybe[Int] = Just(2)
  def main(args: Array[String]): Unit = a match {
    case Just(2) =>
    case Empty() =>
  }
}
