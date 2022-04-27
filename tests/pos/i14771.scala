trait Layouts:
  type Layout <: {
    def withName(name: String): Layout
  }
  val l: Layout

val ls = new Layouts:
  class Layout17:
    def withName(name: String): Layout17 = this
  type Layout = Layout17
  val l = Layout17()

def test = ls.l
