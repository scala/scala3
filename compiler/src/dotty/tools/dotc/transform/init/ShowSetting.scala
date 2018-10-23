package dotty.tools.dotc
package transform
package init

import core._
import Contexts.Context
import collection.mutable

case class ShowSetting(heap: Heap, ctx: Context, printed: mutable.Set[Int] = mutable.Set()) {
  def indent(content: String, tabs: Int = 1): String = ShowSetting.pad(content, tabs, padFirst = true)
}

object ShowSetting {
  val indentTab = " "

  def pad(s: String, tabs: Int = 1, padFirst: Boolean = false) = {
    val padding = indentTab * tabs
    s.split("\n").mkString(if (padFirst) padding else "", "\n" + padding, "")
  }
}