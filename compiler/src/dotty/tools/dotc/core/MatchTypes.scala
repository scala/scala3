package dotty.tools
package dotc
package core

import Types.*, Contexts.*, Symbols.*, Flags.*, Decorators.*

object MatchTypes:

  /* Concreteness checking
   *
   * When following a baseType and reaching a non-wildcard, in-variant-pos type capture,
   * we have to make sure that the scrutinee is concrete enough to uniquely determine
   * the values of the captures. This comes down to checking that we do not follow any
   * upper bound of an abstract type.
   *
   * See notably neg/wildcard-match.scala for examples of this.
   *
   * See neg/i13780.scala, neg/i13780-1.scala and neg/i19746.scala for
   * ClassCastException reproducers if we disable this check.
   */
  def isConcrete(tp: Type)(using Context): Boolean =
    val tp1 = tp.normalized

    tp1 match
      case tp1: TypeRef =>
        if tp1.symbol.isClass then true
        else
          tp1.info match
            case info: AliasingBounds => isConcrete(info.alias)
            case _                    => false
      case tp1: AppliedType =>
        isConcrete(tp1.tycon) && isConcrete(tp1.superType)
      case tp1: HKTypeLambda =>
        true
      case tp1: TermRef =>
        !tp1.symbol.is(Param) && isConcrete(tp1.underlying)
      case _: (ParamRef | MatchType) =>
        false
      case tp1: TypeProxy =>
        isConcrete(tp1.underlying)
      case tp1: AndOrType =>
        isConcrete(tp1.tp1) && isConcrete(tp1.tp2)
      case _ =>
        false
  end isConcrete

end MatchTypes