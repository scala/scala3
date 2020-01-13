object O {
  def m1(a: Int*) = (a: _*) // error: Cannot return repeated parameter type Int*
  def m2(a: Int*) = { // error: Cannot return repeated parameter type Int*
    val b = (a: _*)
    b
  }
  def m3(a: Int*): Any = {
    val b = (a: _*) // Ok, does not affect the signature
    b
  }
  def m4(a: 2*) = (a: _*) // error: Cannot return repeated parameter type Int*

}

class O(a: Int*) {
  val m = (a: _*) // error: Cannot return repeated parameter type Int*
}

