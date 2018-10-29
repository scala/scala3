class A { self : B =>
  val y = f             // error
}

trait B(x: Int) {
   def f: Int = x
}

class C extends A with B(20)

//////////////


class X { self : Y =>
  val y = f
}

trait Y(x: Int) {
   @scala.annotation.icy
   def f: Int = x            // error // error
}

class Z extends A with B(20)
