package dotty.tools
package dotc
package util

import scala.collection.mutable
import core.Names.TermName
import core.UniqueNameKind
import core.StdNames.str

abstract class FreshNameCreator {
  def newName(prefix: TermName, unique: UniqueNameKind): TermName
}

object FreshNameCreator {
  class Default extends FreshNameCreator {
    protected var counter: Int = 0
    protected val counters: mutable.Map[String, Int] = mutable.AnyRefMap() withDefaultValue 0

    /**
     * Create a fresh name with the given prefix. It is guaranteed
     * that the returned name has never been returned by a previous
     * call to this function (provided the prefix does not end in a digit).
     */
    def newName(prefix: TermName, unique: UniqueNameKind): TermName = {
      val key = str.sanitize(prefix.toString) + unique.separator
      counters(key) += 1
      prefix.derived(unique.NumberedInfo(counters(key)))
    }
  }
}
