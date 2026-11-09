import scala.language.implicitConversions

class RichComposite(val comp: MockComposite) extends AnyVal {
  def setRowLayout(margin: Int, spacing: Int, vertical: Boolean = false): MockLayout = ???
}

object Implicits {
  implicit def toRichComposite(c: MockComposite): RichComposite = new RichComposite(c)
}

import Implicits.*

class BuggyClass(parent: MockComposite) {
  val comp = new MockComposite(parent, 0)
  comp.setRowLayout(5, 5).wrap = false  // This triggers the bug
}
