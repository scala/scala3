package dotty.tools.scaladoc.util

object Escape:
  def escapeUrl(url: String) = url
    .replace("#","%23")

  def escapeFilename(filename: String) =
    val escaped = filename
      .replace("/", "$div")
      .replace("\\", "$bslash")
    if escaped != filename then escaped + "$" else escaped
