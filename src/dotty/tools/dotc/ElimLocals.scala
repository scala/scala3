package dotty.tools.dotc
package transform

import core._
import TreeTransforms.{TransformerInfo, TreeTransform, TreeTransformer}
import DenotTransformers._

/** Widens all private[this] and protected[this] qualifiers to just private/protected */
abstract class ElimLocals extends TreeTransform with InfoTransformer { thisTransformer =>

  // TODO complete

}