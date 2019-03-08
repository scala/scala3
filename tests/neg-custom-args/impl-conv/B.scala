package implConv

object B {
  import A._

  "".foo

  val x: Int = ""  // error: feature
  val y: String = 1 // error: feature

}
