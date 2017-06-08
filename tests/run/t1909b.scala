class Ticket1909 (x: Int) {
  def this() = this({
    def bar() = 5
    bar()
  })
}
object Test extends dotty.runtime.LegacyApp {
  new Ticket1909()
}
