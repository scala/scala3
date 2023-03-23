inline trait A:
  inline trait InnerA:
    val x = 1

class B extends A:
  class InnerB extends InnerA
  def f = InnerB().x