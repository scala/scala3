trait T1:
  def x = 0

class Impl1 extends T1

trait T2(val p: String)
class Impl2 extends T2("test") with T1
class Impl3 extends T2(Impl2().p)

@main
def Test: Unit =
  println(Impl1().x) // 0
  println(Impl2().p) // test
  println(Impl3().p) // test
