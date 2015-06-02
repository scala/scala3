class specialization {
  def printer[@specialized(Int, Long) T, U](a: T, b:U) = {
    println(a.toString + b.toString)
  }
}

