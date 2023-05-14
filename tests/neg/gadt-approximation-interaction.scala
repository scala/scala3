object MemberHealing {
  enum SUB[-A, +B]:
    case Refl[S]() extends SUB[S, S]

  def foo[T](t: T, ev: T SUB Int) =
    ev match { case SUB.Refl() =>
      t + 2
    }
}

object ImplicitLookup {
  enum SUB[-A, +B]:
    case Refl[S]() extends SUB[S, S]

  class Tag[T]

  implicit val ti: Tag[Int] = Tag()

  def foo[T](t: T, ev: T SUB Int) =
    ev match { case SUB.Refl() =>
      implicitly[Tag[Int]]
    }
}

object GivenLookup {
  enum SUB[-A, +B]:
    case Refl[S]() extends SUB[S, S]

  class Tag[T]

  given ti: Tag[Int] with {}

  def foo[T](t: T, ev: T SUB Int) =
    ev match { case SUB.Refl() =>
      summon[Tag[Int]]
    }
}

object ImplicitConversion {
  enum SUB[-A, +B]:
    case Refl[S]() extends SUB[S, S]

  class Pow(self: Int):
    def **(other: Int): Int = math.pow(self, other).toInt

  implicit def pow(i: Int): Pow = Pow(i)

  def foo[T](t: T, ev: T SUB Int) =
    ev match { case SUB.Refl() =>
      t ** 2
    }

  def bar[T](t: T, ev: T SUB Int) =
    ev match { case SUB.Refl() =>
      (t: Int) ** 2 // sanity check
    }
}

object GivenConversion {
  enum SUB[-A, +B]:
    case Refl[S]() extends SUB[S, S]

  class Pow(self: Int):
    def **(other: Int): Int = math.pow(self, other).toInt

  given Conversion[Int, Pow] = (i: Int) => Pow(i)

  def foo[T](t: T, ev: T SUB Int) =
    ev match { case SUB.Refl() =>
      t ** 2
    }

  def bar[T](t: T, ev: T SUB Int) =
    ev match { case SUB.Refl() =>
      (t: Int) ** 2 // sanity check
    }
}

object ExtensionMethod {
  enum SUB[-A, +B]:
    case Refl[S]() extends SUB[S, S]

  extension (x: Int)
    def **(y: Int) = math.pow(x, y).toInt

  def foo[T](t: T, ev: T SUB Int) =
    ev match { case SUB.Refl() =>
      t ** 2
    }
}

object HKFun {
  enum SUB[-A, +B]:
    case Refl[S]() extends SUB[S, S]

  enum HKSUB[-F[_], +G[_]]:
    case Refl[H[_]]() extends HKSUB[H, H]

  def foo[F[_], T](ft: F[T], hkev: F HKSUB Option, ev: T SUB Int) =
    hkev match { case HKSUB.Refl() =>
      ev match { case SUB.Refl() =>
        // both should typecheck - we should respect invariance of F
        // (and not approximate its argument)
        // but also T <: Int b/c of ev
        val x: T   = ft.get
        val y: Int = ft.get
      }
    }

  enum COVHKSUB[-F[+_], +G[+_]]:
    case Refl[H[_]]() extends COVHKSUB[H, H]

  def bar[F[+_], T](ft: F[T], hkev: F COVHKSUB Option, ev: T SUB Int) =
    hkev match { case COVHKSUB.Refl() =>
      ev match { case SUB.Refl() =>
        // Sanity check for `foo`
        // this is an error only because we blindly approximate covariant type arguments
        // if it stops being an error, `foo` should be re-thought
        val x: T   = ft.get // error
        val y: Int = ft.get
      }
    }
}

object NestedConstrained {
  enum SUB[-A, +B]:
    case Refl[S]() extends SUB[S, S]

  def foo[A, B](a: A, ev1: A SUB Option[B], ev2: B SUB Int) =
    ev1 match { case SUB.Refl() =>
      ev2 match { case SUB.Refl() =>
        1 + "a"
        a.get : Int
      }
    }
}
