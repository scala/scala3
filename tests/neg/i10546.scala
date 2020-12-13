object test:
  def times(num : Int)(block : => Unit) : Unit = ()
    times(10): println("ah") // error: end of statement expected but '(' found // error

  def foo: Set(Int) = Set(1) // error: end of statement expected but '(' found // error // error
