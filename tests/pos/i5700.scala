// https://github.com/scala/scala3/issues/5700
object noRecursionLimit:
  type M = { type T[+A]; type Ev >: T[Any] <: T[Nothing] }
  val M: M = ().asInstanceOf[M]
  def dcast(m: M.T[Any]): M.T[Int] = m: M.Ev
