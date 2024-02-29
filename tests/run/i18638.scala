type U[H, T] = (Unit, Unit)
object O:
  opaque type U[H, T] <: (Unit, Unit) = (Unit, Unit)
  def u: U[Int, Int] = ((), ())


def test1(u: (Unit, Unit)) = u._1
def test2(u: U[Int, Int]) = u._1
def test3(u: O.U[Int, Int]) = u._1
def test4() =
  (((), ()): U[Int, Int]) match
    case ((), ()) => println("ok")

@main def Test: Unit =
  test1(((), ()))
  test2(((), ()))
  test3(O.u)
  test4()
