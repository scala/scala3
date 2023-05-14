object Test {
  def f(s: String) = s

  def g: (Int, Int) = {
    f("Foo")
      (1, 2)  // error, ok in Scala 2
  }
  def g2: (Int, Int) = {
    f("Foo")
    (1, 2)  // ok, ok in Scala 2
  }

  def h: Unit = {
    f("Foo")
      {}  // error, error in Scala 2
  }

  def i: Unit = {
    f("Foo")
    {}    // ok, error in Scala 2
  }

  def j: Int = {
    return  // error, error in Scala 2
    1 + 2
  }

  def k: Int = {
    return   // ok, error in Scala 2
      1 + 2
  }
}
