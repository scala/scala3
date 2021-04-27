object O {
  def m1(a: Int*) = (a*) // error
  def m2(a: Int*) = {
    val b = (a*) // error
    b
  }
  def m3(a: Int*): Any = {
    val b = (a*) // error
    b
  }
  def m4(a: 2*) = (a*) // error

}

class O(a: Int*) {
  val m = (a*) // error

}