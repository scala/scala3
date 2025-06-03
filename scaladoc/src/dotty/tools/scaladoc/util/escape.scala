package dotty.tools.scaladoc.util

object Escape:
  def escapeUrl(url: String) = url
    .replace("#","%23")

  def escapeFilename(filename: String) =
    // from compiler/src/dotty/tools/dotc/util/NameTransformer.scala
    val escaped = filename
      .replace("~", "$tilde")
      .replace("=", "$eq")
      .replace("<", "$less")
      .replace(">", "$greater")
      .replace("!", "$bang")
      .replace("#", "$hash")
      .replace("%", "$percent")
      .replace("^", "$up")
      .replace("&", "$amp")
      .replace("|", "$bar")
      .replace("*", "$times")
      .replace("/", "$div")
      .replace("+", "$plus")
      .replace("-", "$minus")
      .replace(":", "$colon")
      .replace("\\", "$bslash")
      .replace("?", "$qmark")
      .replace("@", "$at")
    if escaped != filename then escaped + "$" else escaped
