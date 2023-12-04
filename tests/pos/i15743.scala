case class Bar[T <: Tuple](val x: Int *: T)

class Test:
  def fail(e: Any): Int =
    e match { case b: Bar[t] => b.x.head }
