class One[A]{}
def test[A](a: Class[A]) = println(a)
def test[A](as: List[A]) = println(as)
def tost[A](a: Class[A]) = println(a)

@main def main() = {
  val one: One[_] = new One()
  test(one.getClass(): Class[?]) //this fails
  val cls = one.getClass()
  test(cls) //this is ok
  tost(one.getClass()) //this is also ok
}
