object Issue10001 {
  val a: String = "Issue10001"
  val b: String | Null = a
  val c = s"$a"
  val d = s"$b"
}