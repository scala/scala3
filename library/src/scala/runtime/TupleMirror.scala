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

  /** Creates a tuple of this mirror's arity from the elements of `product`.
   *
   *  Delegates to [[scala.runtime.Tuples.fromProduct]], so if `product` already is a
   *  tuple in the representation matching its arity, it is returned as is rather
   *  than copied.
   *
   *  @param product the product supplying the elements of the resulting tuple
   *  @throws IllegalArgumentException if `product.productArity` differs from the
   *          arity this mirror was constructed with
   */
  final def fromProduct(product: Product): MirroredMonoType =
    if product.productArity != arity then
      throw IllegalArgumentException(s"expected Product with $arity elements, got ${product.productArity}")
    runtime.Tuples.fromProduct(product).asInstanceOf[MirroredMonoType]
