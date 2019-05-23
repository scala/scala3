object buffer {
  object EssaInt {
    def unapply(i: Int): Some[Int] = Some(i)
  }

  case class Inv[T](t: T)

  enum EQ[A, B] { case Refl[T]() extends EQ[T, T] }
  enum SUB[A, +B] { case Refl[T]() extends SUB[T, T] } // A <: B

  def test_eq1[A, B](eq: EQ[A, B], a: A, b: B): B =
    Inv(a) match { case Inv(_: Int) => // a >: Sko(Int)
      Inv(a) match { case Inv(_: Int) => // a >: Sko(Int) | Sko(Int)
        eq match { case EQ.Refl() => // a = b
          val success: A = b
          val fail: A = 0 // error
          0 // error
        }
      }
    }

  def test_eq2[A, B](eq: EQ[A, B], a: A, b: B): B =
    Inv(a) match { case Inv(_: Int) => // a >: Sko(Int)
      Inv(b) match { case Inv(_: Int) => // b >: Sko(Int)
        eq match { case EQ.Refl() => // a = b
          val success: A = b
          val fail: A = 0 // error
          0 // error
        }
      }
    }

  def test_sub1[A, B](sub: SUB[A, B], a: A, b: B): B =
    Inv(b) match { case Inv(_: Int) => // b >: Sko(Int)
      Inv(b) match { case Inv(_: Int) => // b >: Sko(Int) | Sko(Int)
        sub match { case SUB.Refl() => // b >: a
          val success: B = a
          val fail: A = 0 // error
          0 // error
        }
      }
    }

  def test_sub2[A, B](sub: SUB[A, B], a: A, b: B): B =
    Inv(a) match { case Inv(_: Int) => // a >: Sko(Int)
      Inv(b) match { case Inv(_: Int) => // b >: Sko(Int) | Sko(Int)
        sub match { case SUB.Refl() => // b >: a
          val success: B = a
          val fail: A = 0 // error
          0 // error
        }
      }
    }


  def test_sub_eq[A, B, C](sub: SUB[A|B, C], eqA: EQ[A, 5], eqB: EQ[B, 6]): C =
    sub match { case SUB.Refl() => // C >: A | B
      eqA match { case EQ.Refl() => // A = 5
        eqB match { case EQ.Refl() => // B = 6
          val fail1: A = 0 // error
          val fail2: B = 0 // error
          0 // error
        }
      }
    }
}
