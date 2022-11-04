trait Encoder[T]
object Encoder:
  def derived[T](using scala.deriving.Mirror.Of[T]): Encoder[T] = new Encoder[T] {}

case object Bar
enum Bar derives Encoder:
  case A, B

@main def Test: Unit =
  summon[Encoder[Bar]]
