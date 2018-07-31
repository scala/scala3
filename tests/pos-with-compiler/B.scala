import dotty.tools.dotc.core.Contexts.Context

object Formatting {
  def rainbows(implicit ctx: Context): String =
    ctx.settings.color.value.toString
}
