package dotty.tools.dotc.interpreter

import dotty.tools.dotc.ast.tpd

/** Quoted `quoted.Quoted` for which its internal representation is its tree.
 *  - Used for trees that cannot be serialized, such as references to local symbols that will be spliced in.
 *  - Used for trees that do not need to be serialized to avoid the overhead of serialization/deserialization.
 */
trait RawQuoted extends quoted.Quoted {
  def tree: tpd.Tree
  override def toString: String = s"${this.getClass.getName}(${tree.toString})"
}
