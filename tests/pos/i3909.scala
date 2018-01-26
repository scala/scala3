class ~(x: Int)
object Test {
  new ~(1)   // Syntax error
  new `~`(1) // Syntax error

  def ~(x: Int) = 1
  ~(1) // OK
}
