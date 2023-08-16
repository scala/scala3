//> using options -Werror
// like the original, but with a case body `a`
// which caused type avoidance to infinitely recurse
sealed trait Parent
final case class Leaf[A, B >: A](a: A, b: B) extends Parent

def run(x: Parent) = x match
  case Leaf(a, _) => a
