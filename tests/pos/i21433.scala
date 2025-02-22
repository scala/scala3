trait A[T]:
  type R = T ?=> Unit
  def f: R = ()

class B extends A[Int]:
  override def f: R = ()
