import annotation.alpha

trait Parent {
  @alpha("add")
  def +=(x: String): Unit
}

class Child extends Parent {
  def +=(x: String) = ()
}

class GrandChild extends Child {
  override def +=(x: String) = ()
}
