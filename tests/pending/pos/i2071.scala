object Test {
  type PF[A, B] = PartialFunction[A, B]

  val f: PF[Int, String] = {
    case i => "bar"
  }
}
