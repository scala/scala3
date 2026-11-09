//> using options -Wunused:all

trait Parent {
  def advanceSpanRange(spanFtIdx: Int): Int = ???
}

object Main extends Parent {
  private var spanFtIdx: Int = 0 // nowarn, read as argument of advanceSpanRange

  def advanceSpanTo(): Unit =
      spanFtIdx = super.advanceSpanRange(spanFtIdx)
}
