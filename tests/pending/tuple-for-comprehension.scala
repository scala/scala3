object Test {
  def main(args: Array[String]) = {
    assert(Some(2) == (for {
      x1 <- Some(1)
      x2 = x1
    } yield x1 + x2))

    assert(Some(3) == (for {
      x1 <- Some(1)
      x2 = x1
      x3 = x1
    } yield x1 + x2 + x3))

    assert(Some(4) == (for {
      x1 <- Some(1)
      x2 = x1
      x3 = x1
      x4 = x1
    } yield x1 + x2 + x3 + x4))

    assert(Some(5) == (for {
      x1 <- Some(1)
      x2 = x1
      x3 = x1
      x4 = x1
      x5 = x1
    } yield x1 + x2 + x3 + x4 + x5))

    assert(Some(6) == (for {
      x1 <- Some(1)
      x2 = x1
      x3 = x1
      x4 = x1
      x5 = x1
      x6 = x1
    } yield x1 + x2 + x3 + x4 + x5 + x6))

    assert(Some(21) == (for {
      x1 <- Some(1)
      x2 = x1
      x3 = x1
      x4 = x1
      x5 = x1
      x6 = x1
      x7 = x1
      x8 = x1
      x9 = x1
      x10 = x1
      x11 = x1
      x12 = x1
      x13 = x1
      x14 = x1
      x15 = x1
      x16 = x1
      x17 = x1
      x18 = x1
      x19 = x1
      x20 = x1
      x21 = x1
    } yield x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21))

    assert(Some(22) == (for {
      x1 <- Some(1)
      x2 = x1
      x3 = x1
      x4 = x1
      x5 = x1
      x6 = x1
      x7 = x1
      x8 = x1
      x9 = x1
      x10 = x1
      x11 = x1
      x12 = x1
      x13 = x1
      x14 = x1
      x15 = x1
      x16 = x1
      x17 = x1
      x18 = x1
      x19 = x1
      x20 = x1
      x21 = x1
      x22 = x1
    } yield x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22))

    assert(Some(23) == (for {
      x1 <- Some(1)
      x2 = x1
      x3 = x1
      x4 = x1
      x5 = x1
      x6 = x1
      x7 = x1
      x8 = x1
      x9 = x1
      x10 = x1
      x11 = x1
      x12 = x1
      x13 = x1
      x14 = x1
      x15 = x1
      x16 = x1
      x17 = x1
      x18 = x1
      x19 = x1
      x20 = x1
      x21 = x1
      x22 = x1
      x23 = x1
    } yield x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23))

    assert(Some(24) == (for {
      x1 <- Some(1)
      x2 = x1
      x3 = x1
      x4 = x1
      x5 = x1
      x6 = x1
      x7 = x1
      x8 = x1
      x9 = x1
      x10 = x1
      x11 = x1
      x12 = x1
      x13 = x1
      x14 = x1
      x15 = x1
      x16 = x1
      x17 = x1
      x18 = x1
      x19 = x1
      x20 = x1
      x21 = x1
      x22 = x1
      x23 = x1
      x24 = x1
    } yield x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24))
  }
}
