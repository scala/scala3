inline trait A:
  object Inner:
    val x = 1

class B extends A:
  def f = Inner.x
