package dotty.tools
package dottydoc
package core

import dotc.CompilationUnit
import dotc.core.Contexts.Context
import dotc.core.Phases.Phase
import model.{Package, Entity}

abstract class JsonOutputPhase extends Phase {
  def phaseName = "jsonOutputPhase"
  println("wabalubadubdub")
}
