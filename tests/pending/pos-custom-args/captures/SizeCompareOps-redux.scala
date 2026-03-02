package collection
trait IterableOps[+A, +CC[_], +C] extends Any:
  def sizeCompare(size: Int): Int

object IterableOps:

  type AnyConstr[X] = Any

  final class SizeCompareOps private[collection](val it: IterableOps[_, AnyConstr, _]^) extends AnyVal:
    this: SizeCompareOps^{it} =>
    /** Tests if the size of the collection is less than some value. */
    @inline def <(size: Int): Boolean = it.sizeCompare(size) < 0
    /** Tests if the size of the collection is less than or equal to some value. */
    @inline def <=(size: Int): Boolean = it.sizeCompare(size) <= 0
    /** Tests if the size of the collection is equal to some value. */
    @inline def ==(size: Int): Boolean = it.sizeCompare(size) == 0
    /** Tests if the size of the collection is not equal to some value. */
    @inline def !=(size: Int): Boolean = it.sizeCompare(size) != 0
    /** Tests if the size of the collection is greater than or equal to some value. */
    @inline def >=(size: Int): Boolean = it.sizeCompare(size) >= 0
    /** Tests if the size of the collection is greater than some value. */
    @inline def >(size: Int): Boolean = it.sizeCompare(size) > 0
