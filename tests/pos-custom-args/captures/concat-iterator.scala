package test

trait IOnce[A]:
  self: I[A]^ =>

trait I[+A]:
  self: I[A]^ =>

  def concat[B >: A](xs: => IOnce[B]^): I[B]^{this, xs} = new I.ConcatI[B](self).concat(xs)

object I:
  private final class ConcatI[+A](val from: I[A]^) extends I[A]:
    override def concat[B >: A](that: => IOnce[B]^): I[B]^{this, that} = ???




