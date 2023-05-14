trait Tst:
  val a: Int

trait Q[A <: Tst]:
  def mk(a: A): a.a.type = a.a

object Q extends Q[Nothing]
