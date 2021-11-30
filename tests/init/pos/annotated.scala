import scala.annotation.nonHotParameters

class WithRef(_b: B) {
  @nonHotParameters def b(b: B) = b.n
}

class WithHot {
  @nonHotParameters final def b(b: B) = b.n
}

class B {
  val n = 10
  val r = WithRef(this)
  val h = WithHot()

  r.b(this)
  h.b(this)
  val m = 15
}
