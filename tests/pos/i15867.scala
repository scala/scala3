enum SUB[-A, +B]:
  case Refl[S]() extends SUB[S, S]

class Pow(self: Int):
  def **(other: Int): Int = math.pow(self, other).toInt

given fromList: [T] => Conversion[List[T], Pow] = ???

given fromInt: Conversion[Int, Pow] = Pow(_)

def foo[T](t1: T, ev: T SUB List[Int]) =
  ev match { case SUB.Refl() =>
    t1 ** 2   // error
  }

def baz[T](t2: T, ev: T SUB Int) =
  ev match { case SUB.Refl() =>
    t2 ** 2   // works
  }
