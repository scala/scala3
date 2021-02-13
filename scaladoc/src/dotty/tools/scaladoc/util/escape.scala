package dotty.tools.scaladoc.util

object Escape:
  def escapeUrl(url: String) = url.replace("#","%23")