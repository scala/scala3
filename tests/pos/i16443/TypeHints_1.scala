// This should be run with Scala 2.13
trait TypeHints {
  val hints: List[Class[?]]
  def components: List[TypeHints] = List(this)

  def + (hints: TypeHints): TypeHints = CompositeTypeHints(components ::: hints.components)

  private case class CompositeTypeHints(override val components: List[TypeHints]) extends TypeHints {
    override val hints: List[Class[?]] = components.flatMap(_.hints)
  }
}

case object NoTypeHints extends TypeHints {
  override val hints = Nil
}