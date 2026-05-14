package scala
package qualifiedTypes

import annotation.experimental

@experimental
object internal:

  /** Placeholder for a free skolem argument reference in a qualified type
   *  annotation. `Owner` is the symbol whose scope the skolem belongs to —
   *  passed as a type-level singleton (typically the `TermRef` of the
   *  enclosing method or class). `index` is the per-owner local identifier.
   *  Only appears inside `@qualified` annotation trees; never called at
   *  runtime. This is the only `ENodeVar` shape that survives encoding to
   *  a tree: bound and opened parameters must already have been substituted
   *  away when `ENode.toTree` runs.
   */
  def skolem[T, Owner](index: Int): T = ???

end internal
