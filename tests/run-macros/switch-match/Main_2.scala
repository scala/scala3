@main def Test: Unit =
  println(f(4))

def f(x: Int): (Int, Seq[String]) =
  unswitch {
    x match
      case 0 => "a"
      case 1 => "b"
  }
