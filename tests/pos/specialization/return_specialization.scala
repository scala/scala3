object return_specialization {
  def qwa[@specialized(Int) T](a: (T, T) => T, b: T): T = {
      if(a ne this) return a(b, b)
      else b
  }
}
