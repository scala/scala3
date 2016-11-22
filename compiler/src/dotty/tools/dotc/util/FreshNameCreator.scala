package dotty.tools
package dotc
package util

import scala.collection.mutable

trait FreshNameCreator {
  def newName(prefix: String = ""): String

  @deprecated("use newName(prefix)", "2.9.0")
  def newName(pos: scala.reflect.internal.util.Position, prefix: String): String = newName(prefix)
  @deprecated("use newName()", "2.9.0")
  def newName(pos: scala.reflect.internal.util.Position): String = newName()
}

object FreshNameCreator {
  class Default extends FreshNameCreator {
    protected var counter = 0
    protected val counters = mutable.AnyRefMap[String, Int]() withDefaultValue 0

    /**
     * Create a fresh name with the given prefix. It is guaranteed
     * that the returned name has never been returned by a previous
     * call to this function (provided the prefix does not end in a digit).
     */
    def newName(prefix: String): String = {
      val safePrefix = prefix.replaceAll("""[<>]""", """\$""")
      counters(safePrefix) += 1
      val counter = counters(safePrefix)
      if (prefix.isEmpty) "$" + counter + "$" else safePrefix + counter
    }
  }
}
