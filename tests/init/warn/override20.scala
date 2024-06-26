class A { self : B =>
  val y = f
}

trait B(x: Int) {  // warn
   def f: Int = x
}

class C extends A with B(20)
