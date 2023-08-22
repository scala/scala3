package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import Hashable.Binders
import util.Spans.Span
import printing.Showable

class RootVar(val source: Symbol) extends CaptureRef, Showable:

  var upperBound: Symbol = NoSymbol
  var lowerBound: Symbol = NoSymbol

  def constrainFromAbove(bound: Symbol)(using Context): Boolean =
    val level = bound.ccNestingLevel
    if !upperBound.exists || upperBound.ccNestingLevel > level then
      if !lowerBound.exists || lowerBound.ccNestingLevel <= level then
        upperBound = bound
        true
      else false
    else true

  def constrainFromBelow(bound: Symbol)(using Context): Boolean =
    val level = bound.ccNestingLevel
    if !lowerBound.exists || lowerBound.ccNestingLevel < level then
      if !upperBound.exists || upperBound.ccNestingLevel >= level then
        lowerBound = bound
        true
      else false
    else true

  override def isRootCapability(using Context) = true

  override def captureSetOfInfo(using Context) = CaptureSet.universal

  def ccNestingLevel(using Context): Int =
    if upperBound.exists then upperBound.ccNestingLevel
    else lowerBound.ccNestingLevel
  def computeHash(bs: Binders): Int = hash
  def hash: Int = System.identityHashCode(this)
  def underlying(using Context): Type = defn.Caps_Root.typeRef

end RootVar

//class LevelError(val rvar: RootVar, val newBound: Symbol, val isUpper: Boolean) extends Exception



