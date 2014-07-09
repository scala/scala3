class C { type T; type U }

trait Test {

  val x: (C { type U = T } { type T = String }) # U
  val y: String = x

}
