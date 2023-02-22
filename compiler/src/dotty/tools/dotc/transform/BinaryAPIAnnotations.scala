package dotty.tools.dotc.transform

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.typer.RefChecks

/** Makes @binaryAPI definitions public */
class BinaryAPIAnnotations extends MiniPhase with SymTransformer:

  override def runsAfterGroupsOf: Set[String] = Set(RefChecks.name)

  override def phaseName: String = BinaryAPIAnnotations.name
  override def description: String = BinaryAPIAnnotations.description

  def transformSym(d: SymDenotation)(using Context): SymDenotation = {
    if d.isBinaryAPI then
      d.resetFlag(Protected)
      d.setPrivateWithin(NoSymbol)
      if d.is(Module) then
        val moduleClass = d.moduleClass
        moduleClass.resetFlag(Protected)
        moduleClass.setPrivateWithin(NoSymbol)
    d
  }

object BinaryAPIAnnotations:
  val name: String = "binaryAPIAnnotations"
  val description: String = "makes @binaryAPI definitions public"
