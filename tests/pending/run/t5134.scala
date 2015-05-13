import language._

object Test extends dotty.runtime.LegacyApp {
  def b = new AnyRef {
    def a= ()
  }
  b.a match { case _ => () }
}
