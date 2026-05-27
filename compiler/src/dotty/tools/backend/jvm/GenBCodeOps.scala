package dotty.tools
package backend
package jvm

object GenBCodeOps {
  extension (flags: Int)
    def addFlagIf(cond: Boolean, flag: Int): Int = if cond then flags | flag else flags
}
