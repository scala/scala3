@main def Test =
  val qs = Seq(2, 3)
  val res = Seq(1, 2, 3) match
    case Seq(b, `qs`: _*) => "matched"
    case Seq(b, vs: _*) => "missed"
  assert(res == "matched", res)
