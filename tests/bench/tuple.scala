object Test {
  val xs0 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  assert(xs0(15) == 16)
    // 2.787s

  val xs1 = xs0 ++ xs0
  assert(xs1(31) == 16)
    // 3.354s
}
