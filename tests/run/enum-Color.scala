enum Color {
  case Red, Green, Blue
}

object Test {
  def main(args: Array[String]) =
    for (color <- Color.values) {
      println(s"$color: ${color.enumTag}")
      assert(Color.valueOf(color.enumTag) eq color)
    }
}
