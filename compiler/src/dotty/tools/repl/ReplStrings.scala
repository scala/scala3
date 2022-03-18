package dotty.tools.repl

import scala.language.unsafeNulls

import scala.annotation.internal.sharable

object ReplStrings {
  // no escaped or nested quotes
  @sharable private val inquotes = """(['"])(.*?)\1""".r
  def unquoted(s: String) = s match { case inquotes(_, w) => w ; case _ => s }
  def words(s: String) = (s.trim split "\\s+" filterNot (_ == "") map (unquoted _)).toList
}
