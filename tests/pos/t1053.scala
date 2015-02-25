trait T[A] { trait U { type W = A; val x = 3 } }
trait Base { type V }

object Test {
  val x : (Base { type V = T[this.type] })#V = null
  val y = new x.U { }
}
