object Test {
  inline def encode(n: String): List[String] =
    for {
      a <- List("s")
      b <- List("w")
      c <- encode(n)
    } yield c

  def encode0(n: String) = encode(n) // error
}
