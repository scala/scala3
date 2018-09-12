object Test extends App {
  val xs0 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  assert(xs0(15) == 16)
    // 2.787s

  val xs1 = xs0 ++ xs0
  assert(xs1(31) == 16)
    // 3.354s

  val xs2 = xs1 ++ xs1
  assert(xs2(63) == 16)
    // 3.523s

  val xs3 = xs2 ++ xs2
  assert(xs3(127) == 16)
    // 3.722s

/* The following operations exhaust the standard stack, but succeed with -Xs10m:

  val xs4 = xs3 ++ xs3
  assert(xs4(255) == 16)
    // 4.023s

  val xs5a = xs3 ++ xs4
  assert(xs5a(383) == 16)
    // 4.243s

  val xs5 = xs4 ++ xs4
  assert(xs5(511) == 16)
    // 4.416s

  val xs6 = xs5 ++ xs5
  assert(xs6(1023) == 16)
    // 4.900s

  val xs7 = xs6 ++ xs6
  assert(xs7(2047) == 16)
    // 5.538s
*/
}
