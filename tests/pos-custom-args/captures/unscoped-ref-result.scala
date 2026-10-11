import caps.fresh

class Ref[T](init: T) extends caps.Mutable:
  var x: T = init
  update def set(x: T) = this.x = x

class Ref2[T](init: T) extends caps.Mutable:
  val x = Ref[T](init)

def test =
  val r4 = () => Ref2(22)
  val _: () => Ref2[Int]^{fresh} = r4
  val rr4 = Ref2(22)
  val s4 = () => rr4
  val y = r4()
  y.x.set(33) // ok

  def r5() = Ref2(33)
  val r6 = () => r5()
  val z = r5()
  z.x.set(33) // ok
  r5().x.set(33) // ok
