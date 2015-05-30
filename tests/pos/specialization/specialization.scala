trait specialization {
  def printer1[@specialized(Int, Long) T](a: T) = {
    println(a.toString)
  }
  def printer2[@specialized(Int, Long) T, U](a: T, b: U) = {
    println(a.toString + b.toString)
  }
  def print(i: Int) = {
    printer1(i)
    println(" ---- ")
    printer2(i,i)
  }
  print(9)
}
