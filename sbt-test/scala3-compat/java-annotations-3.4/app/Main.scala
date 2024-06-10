object Test:
  def main(args: Array[String]): Unit =
    val actual = listAnnots("ScalaUser")
    val expected = List(
      "new JavaAnnot(a = 5, b = _, c = _)",
      "new JavaAnnot(a = 5, b = _, c = _)",
      "new JavaAnnot(a = 5, b = \"foo\", c = _)",
      "new JavaAnnot(a = 5, b = \"foo\", c = 3)",
      "new JavaAnnot(a = 5, b = _, c = 3)",
      "new JavaAnnot(a = 5, b = \"foo\", c = 3)",
      "new JavaAnnot(a = 5, b = \"foo\", c = 3)",
      "new JavaAnnot(a = 5, b = \"foo\", c = _)",
    )
    if actual != expected then
      println("Expected:")
      expected.foreach(println(_))
      println("Actual:")
      actual.foreach(println(_))
      throw new AssertionError("test failed")
  end main
end Test
