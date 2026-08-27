package scala.runtime

import language.experimental.captureChecking

/** A concrete subclass of `scala.deriving.Mirror.Product`, enabling reduction of bytecode size.
 *  as we do not need to synthesize an anonymous Mirror class at every callsite.
 *
 *  @param arity the number of elements in the mirrored tuple type, must be non-negative
 */
final class TupleMirror(arity: Int) extends scala.deriving.Mirror.Product with Serializable:
  assert(arity >= 0) // technically could be used for EmptyTuple also, but it has its own singleton mirror.

  override type MirroredMonoType <: Tuple

  final def fromProduct(product: Product): MirroredMonoType =
    if product.productArity != arity then
      throw IllegalArgumentException(s"expected Product with $arity elements, got ${product.productArity}")
    runtime.Tuples.fromProduct(product).asInstanceOf[MirroredMonoType]
