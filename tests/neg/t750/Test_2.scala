def test =
  val a = Array(1, 2, 3)
  AO_1.f(a) // error
  AO_1.f[Int](a) // error
  AO_2.f(a) // error
  AO_2.f[Int](a) // error
