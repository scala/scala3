package implConv

object B {
  import A.{_, given}

  "".foo

  val x: Int = ""  // ok
  val y: String = 1 // error: feature
}
