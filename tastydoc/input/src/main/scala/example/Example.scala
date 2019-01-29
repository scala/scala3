package example

class Example {
  val a: String = "1"
  def a(
    x: Int
  ): String =
    x.toString
  def a(
    x: Int,
    y: Int
  ): String =
    a(
      x +
      y
    )
}

class ExampleInit() {

}