class Ref[T](init: T) extends caps.Mutable:
  var x: T = init
  update def set(x: T) = this.x = x
  def hide(x: T) = this.set(x) // error
  update def hide2(x: T) = set(x) // ok

  def sneakyHide(x: T) =
    val self = this
    self.set(x)  // error
    val self2: Ref[T]^ = this // error
    self2.set(x)

class Ref2[T](init: T) extends caps.Mutable:
  val x = Ref[T](init)
  def set(x: T) = this.x.set(x) // error
  update def set2(x: T) = this.x.set(x) // ok

def test =
  val r = Ref(22)
  r.set(33) // ok
  val r1: Ref[Int] = Ref(22)
  r1.set(33) // error

  val r2 = Ref2(22)
  r2.x.set(33) // ok
  val r3: Ref2[Int] = Ref2(22)
  r3.x.set(33) // error


