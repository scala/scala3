package example

class SuperA {
  def a(x:Int) = x
}

class SuperB extends SuperA {
  override def a(x:Int) = super.a(x)
}