class specialization {
  def printer[@specialized(Int, Long) T](a: T) = {
    println(a)
  }
}

