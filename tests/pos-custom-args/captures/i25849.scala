import language.experimental.captureChecking

sealed trait Res[+T]:
  def toOption: Option[T] = this match
    case Ok(value) => Some(value)
    case _         => None

case class Ok[+T](value: T) extends Res[T]
case object Bad extends Res[Nothing]

def check[A](obtained: A, clue: => Any = "not equal"): Unit =
  assert(obtained != null, clue)

def test(): Unit =
  val evens = (1 to 5).map(v => if v % 2 == 0 then Ok(v) else Bad).flatMap(_.toOption)
  check(evens)