//> using options -Werror
trait Base:
  type Value
  inline def oov: Option[Option[Value]] = None
  def get: Option[Value]

trait X extends Base:
  override def get: Option[Value] =
    oov match // was: match may not be exhaustive
      case Some(ov) => ov
      case None => None
