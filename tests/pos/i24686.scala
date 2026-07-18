def Test = Seq.empty[DenseMatrix[Double]].reduce(DenseMatrix.horzcat(_, _))

trait Matrix[T]
trait DenseMatrix[T] extends Matrix[T]

object DenseMatrix:
    def horzcat[M, V](matrices: M*)(using OpSet.InPlaceImpl2[DenseMatrix[V], M]): DenseMatrix[V] = ???

object OpSet extends HasOps:
  trait InPlaceImpl2[V1, V2]

trait HasOps
object HasOps extends DenseMatrixExpandedOps with DensMatrixLowPriority

trait DenseMatrixExpandedOps:
  given OpSet.InPlaceImpl2[DenseMatrix[Double], DenseMatrix[Double]] = ???

trait DensMatrixLowPriority extends LowPriorityDenseMatrix1
trait LowPriorityDenseMatrix1:
  given [V]: OpSet.InPlaceImpl2[DenseMatrix[V], Matrix[V]] = ???
