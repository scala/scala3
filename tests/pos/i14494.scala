object ImplNotFound:
  def main(args: Array[String]): Unit =
    val res: Seq[String | Int] = (??? : Seq[Int]).collect {
      case 1 => Seq("")
      case 2 => Seq(1)
    }.flatten