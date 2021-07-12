import scala.annotation.unchecked.uncheckedVariance

final case class Outlet[T](out: T)
final case class SourceShape[+T](val out: Outlet[T @uncheckedVariance])