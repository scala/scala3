sealed trait Stream[+A]{
    import Stream.*;

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def append[B >: A](other : => Stream[B]) : Stream[B] =
      foldRight(other : Stream[B])((elem, stream) => cons(elem, stream))

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
