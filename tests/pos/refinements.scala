trait TS { type TX = Int }
trait TT { self: { type TX = Int } =>
  type TX
  def lift(x: Int): TX = x
}

// A more direct version

trait UU {
  type UX
  val u: UX
  val x: this.type & { type UX = Int }
  val y: Int = x.u
  val z: x.UX = y
}
