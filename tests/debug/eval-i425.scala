// https://github.com/scalacenter/scala-debug-adapter/issues/425
object Test:
  private class Patch(var span: Span)

  def main(args: Array[String]): Unit =
    val patch = new Patch(new Span(0))
    println("ok")

class Span(val start: Int) extends AnyVal:
  def end: Int = start + 1
