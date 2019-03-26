package scala.tasty.reflect

trait ReportingOps extends Core {

  def error(msg: => String, pos: Position)(implicit ctx: Context): Unit =
    kernel.error(msg, pos)

  def error(msg: => String, source: SourceFile, start: Int, end: Int)(implicit ctx: Context): Unit =
    kernel.error(msg, source, start, end)

  def warning(msg: => String, pos: Position)(implicit ctx: Context): Unit =
    kernel.warning(msg, pos)

  def warning(msg: => String, source: SourceFile, start: Int, end: Int)(implicit ctx: Context): Unit =
    kernel.warning(msg, source, start, end)
}
