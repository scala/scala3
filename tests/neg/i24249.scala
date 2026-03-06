

class C(i: Int) {
  def this(s: String) = this(j = s.toInt, i = 27) // error

  def this(i: Int, j: Int) = this(i + j)

  def c = 42 * i
}
