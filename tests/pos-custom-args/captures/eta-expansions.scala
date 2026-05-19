import caps.*

class Cap extends caps.SharedCapability

def test(d: Cap) =
  def m(x: Cap^): Cap^ = x
  val f: (x: Cap^) -> Cap^{fresh} = m       // simple eta expansion
  def map2(xs: List[Int])(f: Int => Int): List[Int] = xs.map(f)
  val f1 = map2                      // capture polymorphic implicit eta expansion
  val f2c: List[Int] => (Int => Int) => List[Int] = f1
  val a0 = identity[Cap ->{d} Unit]  // capture monomorphic implicit eta expansion
  val a0c: (Cap ->{d} Unit) ->{d} Cap ->{d} Unit = a0
  val b0 = (x: Cap ->{d} Unit) => identity[Cap ->{d} Unit](x) // not an implicit eta expansion, hence capture polymorphic
