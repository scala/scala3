class Ref[T](init: T) extends caps.Mutable:
  var fld: T = init
  update def set(x: T) = this.fld = x
  def f(x: T) =
    val self3 = () => this
    val _: () ->{this.rd} Ref[T]^{this.rd} = self3

class Ref2[T](init: T) extends caps.Mutable:
  val x: Ref[T]^ = Ref[T](init)
  update def set(x: T) = this.x.set(x)

class Ref3[T](init: T) extends caps.Mutable:
  val xx = Ref[T](init)
  update def set(x: T) = this.xx.set(x)

def test =
  val r: Ref[Int] = Ref(22)
  val f = () => r.fld
  val _: () ->{r.rd} Int = f
  val f2:  () ->{r.rd} Int = () => r.fld
  val _: () -> Int = f // error

  val r2: Ref2[Int] = Ref2(22)
  val g = () => r2.x.fld
  val _: () ->{r2.x.rd} Int = g
  val _: () -> Int = g // error

  val r3: Ref3[Int] = Ref3(22)
  val h = () => r3.xx.fld
  val _: () ->{r3.xx.rd} Int = h
  val _: () -> Int = h // error



