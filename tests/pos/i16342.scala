type Opaque = Base with Tag

type Base = Any {
  type Hack
}

trait Tag

object Opaque {
  def apply(value: String): Opaque = value.asInstanceOf[Opaque]

  def unapply(userId: Opaque): Option[String] = Option(userId).map(_.value)
  def unappy2(userId: Base with Tag): Option[String] = Option(userId).map(_.value)
}

final implicit class Ops(private val userId: Opaque) extends AnyVal {
  def value: String = userId.asInstanceOf[String]
}