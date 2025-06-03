import language.experimental.captureChecking

trait Iterable[T] { self: Iterable[T]^ =>
  def map[U](f: T => U): Iterable[U]^{this, f}
}

object Test {
  def indentLines(level: Int, lines: Iterable[String]) =
    lines.map(line => line.split("\n").map("  " + _).mkString("\n"))

  def indentErrorMessages(messages: Iterable[String]) =
    indentLines(1, messages)
}
