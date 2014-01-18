object test {

  val x = 2
  val y: Int = math.abs(x)
  
  val xs = List((1, "a"), (2, "b"))
  
  def write(x: String) = ???
  def write(xs: (Int, String)*) = ??? 
  write(xs: _*)

}