package dotty.tools.vulpix

case class TestGroup(name: String) extends AnyVal {
  override def toString: String = name
}
