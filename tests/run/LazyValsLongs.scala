class I {
  object A1
  object A2
  object A3
  object A4
  object A5
  object A6
  object A7
  object A8
  object A9
  object A10
  object A11
  object A12
  object A13
  object A14
  object A15
  object A16
  object A17
  object A18
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new I
    import c.*
    val l1 = List(A1,
      A2,
      A3,
      A4,
      A5, A6,
                  A7,
                  A8,
                  A9,
                  A10,
                  A11,
                  A12,
                  A13,
                  A14,
                  A15,
                  A16,
                  A17,
                  A18)
    val l2 = List(A1,
      A2,
      A3,
                  A4,
                  A5,
                  A6,
                  A7,
                  A8,
                  A9,
                  A10,
                  A11,
                  A12,
                  A13,
                  A14,
                  A15,
                  A16,
                  A17,
                  A18)
    assert(l1.mkString == l2.mkString)
    assert(!l1.contains(null)) // @odersky - 2.12 encoding seems wonky here as well
  }
}
