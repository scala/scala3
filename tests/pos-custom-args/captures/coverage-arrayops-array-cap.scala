//> using options -Yexplicit-nulls -Wsafe-init -language:experimental.captureChecking

abstract class CoverageArraySeq[+A]:
  def unsafeArray: Array[?]

  def reverseArray: CoverageArraySeq[A] =
    CoverageArraySeq.unsafeWrapArray(new scala.collection.ArrayOps(unsafeArray).reverse).asInstanceOf[CoverageArraySeq[A]]

object CoverageArraySeq:
  def unsafeWrapArray[T](x: Array[T]): CoverageArraySeq[T] =
    null.asInstanceOf[CoverageArraySeq[T]]
