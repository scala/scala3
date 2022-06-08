package scala.runtime

/** A concrete subclass of `scala.deriving.Mirror.Product`, enabling reduction of bytecode size.
 *  as we do not need to synthesize an anonymous Mirror class at every callsite.
 */
final class TupleMirror(arity: Int) extends scala.deriving.Mirror.Product with Serializable:
  assert(arity > 0) // EmptyTuple is not a valid `MirroredType` for TupleMirror

  override type MirroredMonoType <: NonEmptyTuple

  final def fromProduct(product: Product): MirroredMonoType =
    if product.productArity != arity then
      throw IllegalArgumentException(s"expected Product with $arity elements, got ${product.productArity}")
    runtime.Tuples.fromProduct(product).asInstanceOf[MirroredMonoType]

  override final def toString: String = s"<tuple-mirror@${Integer.toHexString(hashCode).nn.take(6)}>"
