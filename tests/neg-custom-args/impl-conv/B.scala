package implConv

object B {
  import A._

  "".foo

  val x: Int = ""  // error: feature

}
