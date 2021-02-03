// from stdlib
class Test {
  def printf(text: String, args: Any*): Unit = { System.out.print(text format (args: _ *)) }
}