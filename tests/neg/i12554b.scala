import language.`3.0-migration` // behavior should be the same as for Scala-2
object Test {

  def f(s: String) = s

  def g: (Int, Int) = {
    f("Foo")
      (1, 2)  // ok
  }

  def h: Unit = {
    f("Foo")
      {}  // error
  }

  def i: Unit = {
    f("Foo")
    {}    // error
  }

  def j: Int = {
    return  // error
    1 + 2
  }

  def k: Int = {
    return  // error
      1 + 2
  }
}