object Test {
  def main(args: Array[String]) = {
    val _ = Main.myMacro(List[PartialFunction[Int, String]] {
      case 1 => "x"
    })
  }
}
