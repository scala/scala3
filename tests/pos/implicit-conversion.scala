object Test {
  // a problematic implicit conversion, should we flag it?
  given Conversion[String, Int] {
    def apply(x: String): Int = Integer.parseInt(toString)
  }
}