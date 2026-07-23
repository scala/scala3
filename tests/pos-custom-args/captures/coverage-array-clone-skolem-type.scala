//> using options -Yexplicit-nulls -Wsafe-init -language:experimental.captureChecking

abstract class ArrayCloneSeq[A]:
  def array: Array[?]

  def cloneArray: ArrayCloneSeq[A] =
    ArrayCloneSeq.unsafeWrapArray(array.clone()).asInstanceOf[ArrayCloneSeq[A]]

object ArrayCloneSeq:
  def unsafeWrapArray[T](array: Array[T]): ArrayCloneSeq[T] =
    null.asInstanceOf[ArrayCloneSeq[T]]
