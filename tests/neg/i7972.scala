object O {
  def m1(a: Int*) = (a: _*) // error // error: Cannot return repeated parameter type Int*
  def m2(a: Int*) = { // error: Cannot return repeated parameter type Int*
    val b = (a: _*) // error // error: Cannot return repeated parameter type Int*
    b
  }
  def m3(a: Int*): Any = {
    val b = (a: _*) // error // error: Cannot return repeated parameter type Int*
    b
  }
  def m4(a: 2*) = (a: _*) // error // error: Cannot return repeated parameter type Int*

}

class O(a: Int*) {
  val m = (a: _*) // error // error: Cannot return repeated parameter type Int*
}

