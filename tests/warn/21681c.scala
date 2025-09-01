object Test:
  def age: Int = ???
  def age_=(x: Int): Unit = ()
  age = 29
  (age = 29) // warn
