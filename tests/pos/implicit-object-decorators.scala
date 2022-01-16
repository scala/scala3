
final class EitherObjectOps(private val either: Either.type) extends AnyVal {
  def unit[A]: Either[A, Unit] = ???
}

trait EitherSyntax {
  implicit final def catsSyntaxEitherObject(either: Either.type): EitherObjectOps =
    new EitherObjectOps(either)
}
object either extends EitherSyntax


object Test:
  import either.given

  val x = Either.unit

