//> using options -explain

case class Data[A]()

trait TCl[A, B]

object Matches:
  def unapply[A](adt: Data[?])[B](using
      ft: TCl[A, B]
  ): Option[Data[A]] = None

given TCl[Unit, String] = new TCl {}

def main =
  Data() match
    case Matches[Unit](x) => println(x) // error // error
