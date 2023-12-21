trait A {
  var a = 20       // warn
  def f: Int = a
}

class B { self : A =>
  a = 30
  val b = f
}

class C extends B with A