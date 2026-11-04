// The implicitly declared `valueOf` of a Java enum parsed from source names its parameter `name`, as javac does (JLS 8.9.3)
object Test {
  def f = E.valueOf(name = "A")
}
