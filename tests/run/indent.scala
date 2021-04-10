@main def Test =
  val x = false
  val y = 1
  val result =
    x
    || y.match
        case 1 => false
        case 3 => false
        case _ => true
    || !x
  assert(result)

