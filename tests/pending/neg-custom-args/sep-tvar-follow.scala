import language.experimental.captureChecking
trait Ref
def swap(x1: Ref^, x2: Ref^): Unit = ()
def foo(a: Ref^)[X](op: (z: Ref^) -> X^{z}): X^{a} = op(a)
def test1(a: Ref^): Unit =
  def bad(x: Ref^)(y: Ref^{a}): Unit = swap(x, y)
  val t1 = bad
  def t2[X] = foo(a)[X]
  val t3 = t2[(y: Ref^{a}) -> Unit](t1)
  t3(a)  // boom
