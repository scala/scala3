//> using options -Werror
sealed trait Parent
final case class Leaf[A, B >: A](a: A, b: B) extends Parent

def run(x: Parent): Unit = x match {
  case Leaf(a, b) =>
}
