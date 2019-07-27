package dotty.tools.dotc
package typer

import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import ast.{untpd, tpd}

object MemoizeGivenAliases {

  /** Flags that disable caching */
  val NoCacheFlags =
    StableRealizable |  // It's a simple forwarder, leave it as one
    Exported            // Export forwarders are never cached
}

trait MemoizeGivenAliases { this: Typer =>
  import tpd._

  /** Ensure that the right hand side of a parameterless given alias
   *  is cached. This applies to all given aliases that have neither type parameters
   *  nor a given clause. Example: The given alias
   *
   *      given a as TC = rhs
   *
   *  is desugared to
   *
   *      given def a: TC = rhs
   *
   *  It is then expanded as follows:
   *
   *  1. If `rhs` is a simple name `x` (possibly with a `this.` prefix),
   *     or `rhs` is a splice, leave the definition as is.
   *  2. Otherwise, wrap `rhs` in a call to `scala.compiletime.memo`.
   */
  def memoizeGivenAlias(rhs: Tree, meth: Symbol) given Context : Tree = meth.info match {
    case ExprType(rhsType) if meth.is(Given, butNot = MemoizeGivenAliases.NoCacheFlags) =>
      // If rhs is a simple stable TermRef, leave as is.
      val needsMemo = rhs match {
        case SplicedRHS(_) => false
        case _ => rhs.tpe match {
          case rhsTpe @ TermRef(pre, _) if rhsTpe.isStable =>
            pre match {
              case NoPrefix => false
              case pre: ThisType => pre.cls != meth.owner.enclosingClass
              case _ => true
            }
          case _ => true
        }
      }
      if (needsMemo) {
        val memoized = ref(defn.Compiletime_memo).appliedToType(rhsType).appliedTo(rhs)
        adapt(memoized, rhsType) // this will do the inlining of `memo`
      }
      else rhs
    case _ => rhs
  }
}

