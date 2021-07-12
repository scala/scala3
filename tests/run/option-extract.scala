
enum Option[+A]:
  case Some(x: A)
  case None

  opaque type ExtractResult[B] = (=> B) => B

  def extract[B](f: A => B): ExtractResult[B] =
    def result(default: => B): B = (this: Option[A]) match
      case None => default
      case Some(elem) => f(elem)
    result

  extension [B](er: ExtractResult[B])
    def orElse(default: => B): B = er(default)
end Option

@main def Test =
  val x = Option.Some(11)
  val y = x.extract(x => x + 1)
   .orElse(22)
  assert(y == 12, y)
