package dotty.tools.dotc.core

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.Type

/** Phantom erasure erases:
 *
 *  - Parameters/arguments are removed from the function definition/call in `PhantomArgumentEval`.
 *    If the evaluation of the phantom arguments may produce a side effect, these are evaluated and stored in
 *    local `val`s and then the non phantoms are used in the Apply. Phantom `val`s are then erased to
 *    `val ev$i: ErasedPhantom = myPhantom` intended to be optimized away by local optimizations. `myPhantom` could be
 *    a reference to a phantom parameter, a call to Phantom assume or a call to a method that returns a phantom.
 *  - Definitions of `def`, `val`, `lazy val` and `var` returning a phantom type to return a ErasedPhantom. Where fields
 *    with ErasedPhantom type are not memoized (see transform/Memoize.scala).
 *  - Calls to Phantom.assume become calls to ErasedPhantom.UNIT. Intended to be optimized away by local optimizations.
 */
object PhantomErasure {

  /** Returns the default erased type of a phantom type */
  def erasedPhantomType(implicit ctx: Context): Type = defn.ErasedPhantomType

  /** Returns the default erased tree for a call to Phantom.assume */
  def erasedAssume(implicit ctx: Context): Tree = ref(defn.ErasedPhantom_UNIT)

  /** Returns the default erased tree for a phantom parameter ref */
  def erasedParameterRef(implicit ctx: Context): Tree = ref(defn.ErasedPhantom_UNIT)

}
