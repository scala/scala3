abstract class A {
  def f: Int
}

class B extends A {
  val x = f

  def f: Int = 20
}

class C extends A {
  val x = f
  val y = x        // warn

  def f: Int = y
}

class D extends A {
  val x = 10
  val y = f

  def f: Int = x   // ok
}