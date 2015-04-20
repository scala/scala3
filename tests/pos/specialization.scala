class specialization {
  def printer1[@specialized(Int, Long) T](a: T) = {
    println(a.toString)
  }

  def printer2[@specialized(Int, Long) T, U](a: T, b: U) = {
    println(a.toString + b.toString)
  }
  def print(a: Int) = {
    printer1(a)
    println(" ---- ")
    printer2(a,a)
  }
  print(9)
}
