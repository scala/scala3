trait A {
  var a = 20
  def f: Int = 30
}

class B { self : A =>
  a = 30
  val b = f    // error
}