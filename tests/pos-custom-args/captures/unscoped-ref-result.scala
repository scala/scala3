class Ref[T](init: T) extends caps.Mutable:
  var x: T = init
  update def set(x: T) = this.x = x

class Ref2[T](init: T) extends caps.Mutable:
  val x = Ref[T](init)

def test =
  val r4 = () => Ref2(22)
  val _: Ref2[Int]^{r4*} = r4()
  r4().x.set(33) // ok
