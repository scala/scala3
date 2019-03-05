package scala.tasty.reflect

trait ReportingOps extends Core {

  def error(msg: => String, pos: Position)(implicit ctx: Context): Unit =
    kernel.error(msg, pos)

  def warning(msg: => String, pos: Position)(implicit ctx: Context): Unit =
    kernel.warning(msg, pos)

}
