import language.experimental.modularity

// First version: higher-kinded self type
object v1:
  trait Set:
    type Self[A]
    def empty[A]: Self[A]
    def union[A](self: Self[A], other: Self[A]): Self[A]

  case class ListSet[A](elems: List[A])

  given ListSet is Set:
    def empty[A]: ListSet[A] = ListSet(Nil)

    def union[A](self: ListSet[A], other: ListSet[A]): ListSet[A] =
      ListSet(self.elems ++ other.elems)

  def listUnion[A, S[_]: Set](xs: List[S[A]]): S[A] =
    xs.foldLeft(S.empty)(S.union)

  val xs = ListSet(List(1, 2, 3))
  val ys = ListSet(List(4, 5))
  val zs = listUnion(List(xs, ys))

  // Second version: parameterized type class
object v2:
  trait Set[A]:
    type Self
    def empty: Self
    extension (s: Self) def union (other: Self): Self

  case class ListSet[A](elems: List[A])

  given [A] => ListSet[A] is Set[A]:
    def empty: ListSet[A] = ListSet(Nil)

    extension (self: ListSet[A]) def union(other: ListSet[A]): ListSet[A] =
      ListSet(self.elems ++ other.elems)

  def listUnion[A, S: Set[A]](xs: List[S]): S =
    xs.foldLeft(S.empty)(_ `union` _)

  val xs = ListSet(List(1, 2, 3))
  val ys = ListSet(List(4, 5))
  val zs = listUnion(List(xs, ys))

