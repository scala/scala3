package dotty.tools.dotc
package util

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.config.Feature
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.transform.SymUtils._

object Experimental:
  import tpd._

  def annotateExperimental(sym: Symbol)(using Context): Unit =
    if sym.is(Enum) && sym.hasAnnotation(defn.ExperimentalAnnot) then
      // Add @experimental annotation to enum class definitions
      val compMod = sym.companionModule.moduleClass
      compMod.addAnnotation(defn.ExperimentalAnnot)
      compMod.companionModule.addAnnotation(defn.ExperimentalAnnot)
