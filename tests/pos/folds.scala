
object Test:
  extension [A](xs: List[A])
    def foldl[B](acc: B)(f: (A, B) => B): B = ???

  val xs = List(1, 2, 3)

  val _ = xs.foldl(List())((y, ys) => y :: ys)

  val _ = xs.foldl(Nil)((y, ys) => y :: ys)

  def partition[a](xs: List[a], pred: a => Boolean): Tuple2[List[a], List[a]] = {
    xs.foldRight/*[Tuple2[List[a], List[a]]]*/((List(), List())) {
      (x, p) => if (pred (x)) (x :: p._1, p._2) else (p._1, x :: p._2)
    }
  }

  def snoc[A](xs: List[A], x: A) = x :: xs

  def reverse[A](xs: List[A]) =
    xs.foldLeft(Nil)(snoc)

  def reverse2[A](xs: List[A]) =
    xs.foldLeft(List())(snoc)

  val ys: Seq[Int] = xs
  ys.foldLeft(Seq())((ys, y) => y +: ys)
  ys.foldLeft(Nil)((ys, y) => y +: ys)

  def dup[A](xs: List[A]) =
    xs.foldRight(Nil)((x, xs) => x :: x :: xs)

  def toSet[A](xs: Seq[A]) =
    xs.foldLeft(Set.empty)(_ + _)
