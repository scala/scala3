abstract class A {
  def f: Int

  (1 to 10).foreach { i =>
    f
  }

  private val a = 10
}

class B extends A {
  private val a = 30  // error
  def f: Int = a
}