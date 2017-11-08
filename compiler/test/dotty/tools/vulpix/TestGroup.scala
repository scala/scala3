package dotty.tools.vulpix

class TestGroup(val name: String) extends AnyVal {
  override def toString: String = name
}

object TestGroup {
  def apply(name: String): TestGroup = new TestGroup(name)
}
