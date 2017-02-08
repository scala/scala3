enum Color {
  case Red
  case Green
  case Blue
}

object Test {
  def main(args: Array[String]) =
    for (color <- Color.values) {
      println(s"$color: ${color.enumTag}")
      assert(Color.valueOf(color.enumTag) eq color)
    }
}
