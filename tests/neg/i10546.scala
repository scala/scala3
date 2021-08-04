object test:
  def times(num : Int)(block : => Unit) : Unit = ()
    times(10): println("ah") // error: end of statement expected but '(' found

  def foo: Set(Int) = Set(1)