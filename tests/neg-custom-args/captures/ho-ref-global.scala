class Ref

val a: Object^ = ???
def test() =
  val r: Ref^{a} = ???
  def mk1(op: (z: Ref^) -> Ref^{a} ->{z} Unit) = op(r) // error
  def bad(x: Ref^)(y: Ref^{a}) = ???
  mk1(bad)
