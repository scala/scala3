class Ref

def test(a: Object^) =
  val r: Ref^{a} = ???
  def mk1(op: (z: Ref^) -> Ref^{a} ->{z} Unit) = op(r) // error
  def bad(x: Ref^)(y: Ref^{a}) = ???
  mk1(bad)
