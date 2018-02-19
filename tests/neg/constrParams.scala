

class C {
  type INT = Int
  class I
  def this(x: INT) = this() // error: illegal access
  def this(x: I) = this() // error: illegal access
}
