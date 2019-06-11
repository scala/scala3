enum Color {
  case Red, Green, Blue
  class Color  // Just to throw a spanner in the works
}

object Test {
  def main(args: Array[String]) =
    for (color <- Color.values) {
      println(s"$color: ${color.toString}")
      assert(Color.valueOf(color.toString) eq color)
      import Color._
      color match {
        case Red | Green | Blue =>
      }
    }
}
