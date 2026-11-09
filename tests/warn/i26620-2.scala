//> using options -Wunused:all

trait Parent {
  def advanceSpanRange(spanFtIdx: Int): Int = ???
}

object Main extends Parent {
  private var spanFtIdx: Int = 0 // warn, mutated but never read
  private var otherVar: Int = 0 // nowarn, mutated and read as argument
  def advanceSpanTo(): Unit =
    spanFtIdx = super.advanceSpanRange(otherVar)
    otherVar = 1
}
