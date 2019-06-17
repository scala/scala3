

trait Iterator {

  def span() = {
    val self: Int = 33
    class Leading {
      def finish(): Unit = println("finished")
    }
    val leading = new Leading

    class Trailing {
      lazy val it = leading.finish()
    }
    val trailing = new Trailing
    (leading, trailing)
  }
}
