trait TC
given TC = new TC {}
class Head {
  def m(p: (String, String)): Unit = ()
  def m(implicit e: TC): Unit = ()
}
@main def run(): Unit = (new Head).m("a", "b")
