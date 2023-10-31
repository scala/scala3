object G {
  def unapply(m: Any): Option[?] = Some("")
}

object H {
  def unapplySeq(m: Any): Option[Seq[?]] = None
}

object Test {
  (0: Any) match {
    case G(v) => v
    case H(v) => v
    case _ =>
  }
}
