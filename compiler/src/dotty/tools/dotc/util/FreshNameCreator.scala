package dotty.tools
package dotc
package util

import scala.collection.mutable
import core.Names.TermName
import core.NameKinds.UniqueNameKind
import core.StdNames.str

abstract class FreshNameCreator {
  def newName(prefix: TermName, unique: UniqueNameKind): TermName
  def currentCount(prefix: TermName, unique: UniqueNameKind): Int
}

object FreshNameCreator {
  class Default extends FreshNameCreator {
    protected val counters: mutable.Map[String, Int] = mutable.AnyRefMap() withDefaultValue 0

    private def keyFor(prefix: TermName, unique: UniqueNameKind) =
      str.sanitize(prefix.toString) + unique.separator

    /** The current counter for the given combination of `prefix` and `unique` */
    def currentCount(prefix: TermName, unique: UniqueNameKind): Int =
      counters(keyFor(prefix, unique))

    /**
     * Create a fresh name with the given prefix. It is guaranteed
     * that the returned name has never been returned by a previous
     * call to this function (provided the prefix does not end in a digit).
     */
    def newName(prefix: TermName, unique: UniqueNameKind): TermName = {
      val key = keyFor(prefix, unique)
      counters(key) += 1
      prefix.derived(unique.NumberedInfo(counters(key)))
    }
  }
}
