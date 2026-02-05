// scalajs: --skip

@main def Test =
  val res = Array.range(2, 5, Int.MaxValue)
  assert(res.length == 1, s"array length was ${res.length}")
  assert(res.head == 2, s"array(0) was ${res.head}")
