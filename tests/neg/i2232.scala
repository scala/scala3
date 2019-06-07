object Cats {
  trait Trivial[A]
  implicit def trivial[A]: Trivial[A] = new Trivial[A] { }

  type Obj[C[_[_[_], _[_, _]]]] =
    [A]    =>> C[({type l[c0[_], c1[_, _]] = c0[A]   })#l]
  type Cat[C[_[_[_], _[_, _]]]] =
    [A, B] =>> C[({type l[c0[_], c1[_, _]] = c1[A, B]})#l]

  trait Category[C[_[_[_], _[_, _]]]] {
    type ->  = Cats.Cat[C]
    type Obj = Cats.Obj[C]

    def id[A: Obj]: A -> A
    def andThen[A, B, C](ab: A -> B, bc: B -> C): A -> C
  }

  object Category {
    type ByF[F[_, _]] = Category[_] { type -> = F }
  }

  type Scal[f[_[_], _[_, _]]] = f[Trivial, Function1]

  implicit val scal: Category[Scal] = new Category[Scal] {
    def id[A: Obj]: A -> A = a => a
    def andThen[A, B, C](ab: A -> B, bc: B -> C): A -> C = ab.andThen(bc)
  }

  implicit class CategoryOps[F[_, _], A, B](ab: F[A, B]) {
    def >>>[C](bc: F[B, C])(implicit F: Category.ByF[F]): F[A, C] =
      F.andThen(ab, bc)
  }

  val f: Int => Int = _ + 1
  val g: Int => String = _.toString
  f >>> g   // error: no implicit arg found
}
