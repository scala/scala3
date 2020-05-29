sealed trait Test[F[_]]
object Test {
  final case class Completed[F[_]]() extends Test[F]
}
