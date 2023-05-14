object ImplNotFound:
  class TOP
  class STR(s: String) extends TOP
  class INT(i: Int) extends TOP
  def main(args: Array[String]): Unit =
    val res: Seq[STR | INT] = (??? : Seq[Int]).collect {
      case 1 => Seq(STR(""))
      case 2 => Seq(INT(1))
    }.flatten