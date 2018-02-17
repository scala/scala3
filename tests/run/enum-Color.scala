enum Color {
  case Red, Green, Blue
  class Color  // Just to throw a spanner in the works
}

object Test {
  def main(args: Array[String]) =
    for (color <- Color.enumValues) {
      println(s"$color: ${color.enumTag}")
      assert(Color.enumValue(color.enumTag) eq color)
      import Color._
      color match {
        case Red | Green | Blue =>
      }
    }
}
