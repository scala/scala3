case class Box[V](value: V)
object Box:
  def apply[A](a: A): Box[A]        = new Box[A](a)
  def unapply[U](b: Box[U]): Box[U] = b

class Test:
  def value: Box[_ <: String] = Box("text")

  def test: String = value match
    case Box(text) => text: String
