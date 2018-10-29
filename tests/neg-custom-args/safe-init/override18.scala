abstract class A {
  def f: Int
}

class B extends A {
  val x = f

  def f: Int = 20
}

class C extends A {
  val x = f        // error
  val y = x

  def f: Int = y   // error
}

class D extends A {
  val x = 10
  val y = f

  def f: Int = x   // ok
}