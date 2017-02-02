package dotty.tools.dotc

import core.StdNames.nme
import parsing.{precedence, minPrec, maxPrec, minInfixPrec}
import util.Property.Key

package object printing {

  type Precedence = Int

  val DotPrec       = parsing.maxPrec
  val AndPrec       = parsing.precedence(nme.raw.AMP)
  val OrPrec        = parsing.precedence(nme.raw.BAR)
  val InfixPrec     = parsing.minInfixPrec
  val GlobalPrec    = parsing.minPrec
  val TopLevelPrec  = parsing.minPrec - 1

  /** A property to indicate whether the compiler is currently doing -Xprint
   *
   *  -Xprint will print `sym.name` instead of `sym.originalName`
   */
  val XprintMode = new Key[Unit]
}
