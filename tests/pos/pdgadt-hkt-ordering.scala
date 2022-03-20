object test {
    trait Tag { type F[_] }

    enum SubK[-A[_], +B[_]]:
      case Refl[F[_]]() extends SubK[F, F]

    def foo(p: Tag, q: Tag, e: SubK[p.F, q.F]) = p match
      case _: Tag => q match
        case _: Tag => e match
          case SubK.Refl() =>
            val t: q.F[Int] = ??? : p.F[Int]
  }
