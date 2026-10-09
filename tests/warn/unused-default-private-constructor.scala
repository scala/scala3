//> using options -Wunused:all

case class Foo private (s: String, int: Int = 42) { // warn
  def getInt: Int = int
}
