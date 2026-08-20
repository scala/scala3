package scala.collection

import scala.language.experimental.captureChecking

trait SeqView[+A]:
  def iterator: Iterator[A]^{this}
  def sorted[B >: A](implicit ord: Ordering[B]): SeqView[A]^{this} = this

object SeqView:
  private type SomeSeqOps[+A] = SeqOps[A, AnyConstr, ?]

  final class Sorted[A, B >: A] private (
    underlying_ : SomeSeqOps[A]^,
    private val len: Int,
    private val ord: Ordering[B]
  ) extends SeqView[A]:
    outer: Sorted[A, B]^ =>

    private var underlying = underlying_

    def this(underlying: SomeSeqOps[A]^, ord: Ordering[B]) =
      this(underlying, underlying.length, ord)

    def iterator: Iterator[A]^{this} = underlying.iterator

    private def elems: SomeSeqOps[A]^{this} = underlying

    private final class ReverseSorted extends SeqView[A]:
      def iterator: Iterator[A]^{this} = underlying.iterator

      override def sorted[B1 >: A](implicit ord1: Ordering[B1]): SeqView[A]^{this} =
        if ord1 == Sorted.this.ord then outer
        else if ord1.isReverseOf(Sorted.this.ord) then this
        else new Sorted(elems, len, ord1)
