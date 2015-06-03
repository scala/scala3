object return_specialization {
  def qwa[@specialized T](a: (String, String) => T, b: T): T = {
      if(a ne this) return a("1", "2")
      else b
  }
}
