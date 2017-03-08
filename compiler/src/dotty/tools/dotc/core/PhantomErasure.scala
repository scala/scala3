package dotty.tools.dotc.core

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.Type

/** Phantom erasure erases (minimal erasure):
 *
 *  - Parameters/arguments are erased to BoxedUnit. The next step will remove the parameters
 *    from the method definitions and calls (implemented in branch implement-phantom-types-part-2).
 *  - Definitions of `def`, `val`, `lazy val` and `var` returning a phantom type to return a BoxedUnit. Where fields
 *    with BoxedUnit type are not memoized (see transform/Memoize.scala).
 *  - Calls to Phantom.assume become calls to BoxedUnit. Intended to be optimized away by local optimizations.
 *
 *  BoxedUnit is used as it fits perfectly and homogeneously in all locations where phantoms can be found.
 *  Additionally some of the optimizations that can be performed on phantom types can also be directly implemented
 *  on all boxed units.
 */
object PhantomErasure {

  /** Returns the default erased type of a phantom type */
  def erasedPhantomType(implicit ctx: Context): Type = defn.BoxedUnitType

  /** Returns the default erased tree for a call to Phantom.assume */
  def erasedAssume(implicit ctx: Context): Tree = ref(defn.BoxedUnit_UNIT)

}
