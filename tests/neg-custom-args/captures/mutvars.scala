class Ref[T](init: T) extends caps.Mutable:
  var fld: T = init
  def hide(x: T) = this.fld = x // error
  update def hide2(x: T) = this.fld = x // ok

  def sneakyHide(x: T) =
    val self = this
    self.fld = x  // error
    val self2: Ref[T]^ = this // error
    self2.fld = x

    val self3 = () => this
    self3().fld = x  // error
    val self4: () => Ref[T]^ = () => this // error
    self4().fld = x

    def self5() = this
    self5().fld = x  // error
    def self6(): Ref[T]^ = this // error
    self6().fld = x

class Ref2[T](init: T) extends caps.Mutable:
  val x = Ref[T](init)
  def set(x: T) = this.x.fld = x // error
  update def set2(x: T) = this.x.fld = x // ok

def test =
  val r = Ref(22)
  r.fld = 33 // ok
  val r1: Ref[Int] = Ref(22)
  r1.fld = 33 // error

  val r2 = Ref2(22)
  r2.x.fld = 33 // ok
  val r3: Ref2[Int] = Ref2(22)
  r3.x.fld = 33 // error

  val r4 = () => Ref2(22)
  r4().x.fld = 33 // ok
  val ref2: Ref2[Int] = Ref2(22)
  val r5: () => Ref2[Int]^ = () => ref2 // error
  r5().x.fld = 33
  val r6: () => Ref2[Int] = () => ref2
  r6().x.fld = 33 // error
