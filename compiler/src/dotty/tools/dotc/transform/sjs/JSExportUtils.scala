package dotty.tools.dotc
package transform
package sjs

import core._
import NameKinds.DefaultGetterName
import Names._


/** Utilities for JS exports handling. */
object JSExportUtils {
  private inline val ExportPrefix = "$js$exported$"
  private inline val MethodExportPrefix = ExportPrefix + "meth$"
  private inline val PropExportPrefix = ExportPrefix + "prop$"

  /** Creates a name for an export specification. */
  def makeExportName(jsName: String, isProp: Boolean): TermName = {
    val prefix = if (isProp) PropExportPrefix else MethodExportPrefix
    termName(prefix + jsName)
  }

  /** Is this symbol an export forwarder? */
  def isExportName(name: Name): Boolean =
    name.startsWith(ExportPrefix) && !name.is(DefaultGetterName)

  /** Retrieves the originally assigned jsName of this export and whether it is a property. */
  def exportNameInfo(name: Name): (String, Boolean) = {
    val nameString = name.toString()
    if (nameString.startsWith(MethodExportPrefix))
      (nameString.substring(MethodExportPrefix.length), false)
    else if (nameString.startsWith(PropExportPrefix))
      (nameString.substring(PropExportPrefix.length), true)
    else
      throw new IllegalArgumentException(s"non-exported name passed to jsExportInfo: $name")
  }
}
