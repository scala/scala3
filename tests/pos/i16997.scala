class Funs {
  sealed trait ->[A, B]
}

/**
 * Binary tree with leafs holding values of types `F[X]`, `F[Y]`, ...
 * The complete structure of the tree is expressed by the type `A`, using the tags for branches and leafs.
 *
 * @tparam <*> tag for branches
 * @tparam T tag for leafs.
 * @tparam F value type of leafs. Each leaf holds a value of type `F[T]`, for some type `T`.
 * @tparam A captures the complete structure of the tree
 */
enum Tree[<*>[_, _], T[_], F[_], A] {
  case Branch[<*>[_, _], T[_], F[_], A, B](
    l: Tree[<*>, T, F, A],
    r: Tree[<*>, T, F, B],
  ) extends Tree[<*>, T, F, A <*> B]

  case Leaf[<*>[_, _], T[_], F[_], A](
    value: F[A],
  ) extends Tree[<*>, T, F, T[A]]

  def <*>[B](that: Tree[<*>, T, F, B]): Tree[<*>, T, F, A <*> B] =
    Branch(this, that)

  def partition[G[_], H[_]](
    f: [x] => F[x] => Either[G[x], H[x]],
  )(using
    funs: Funs,
  ): Partitioned[G, H, funs.->] =
    this match {
      case Leaf(a) =>
        f(a) match
          case Left(a)  => Partitioned.Left(Leaf(a))
          case Right(a) => Partitioned.Right(Leaf(a))
      case Branch(l, r) =>
        import Partitioned.{Both, Left, Right}
        import l.Partitioned.{Both => LBoth, Left => LLeft, Right => LRight}
        import r.Partitioned.{Both => RBoth, Left => RLeft, Right => RRight}

        (l.partition(f), r.partition(f)) match
          case (LLeft(lg),     RLeft(rg))     => Left(lg <*> rg)
          case (LLeft(lg),     RRight(rh))    => Both(lg, rh)
          case (LLeft(lg),     RBoth(rg, rh)) => Both(lg <*> rg, rh)
          case (LRight(lh),    RLeft(rg))     => Both(rg, lh)
          case (LRight(lh),    RRight(rh))    => Right(lh <*> rh)
          case (LRight(lh),    RBoth(rg, rh)) => Both(rg, lh <*> rh)
          case (LBoth(lg, lh), RLeft(rg))     => Both(lg <*> rg, lh)
          case (LBoth(lg, lh), RRight(rh))    => Both(lg, lh <*> rh)
          case (LBoth(lg, lh), RBoth(rg, rh)) => Both(lg <*> rg, lh <*> rh)
    }

  // note that `->` is never even used, to keep this reproduction case small
  enum Partitioned[G[_], H[_], ->[_, _]] {
    case Left(value: Tree[<*>, T, G, A])
    case Right(value: Tree[<*>, T, H, A])
    case Both[G[_], H[_], X, Y, ->[_, _]](
      l: Tree[<*>, T, G, X],
      r: Tree[<*>, T, H, Y],
    ) extends Partitioned[G, H, ->]
  }
}
