class C { type T; type U }

trait Test {

  val x1: (C { type U = T; type T = String }) # U
  val x2: (C { type U = T } {type T = String }) # U

  val y1: String = x1
  val y2: String = x2
}
