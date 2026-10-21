sealed trait Parent:
  sealed trait Inner[T]
object ConcreteParent extends Parent:
  val myInner: Inner[Boolean] = ???
  def foo(parent: Parent, isFoo: Boolean)(f: parent.type => parent.Inner[Boolean]) = ???
  foo(parent = ConcreteParent, isFoo = true)(_.myInner)
  foo(isFoo = true, parent = ConcreteParent)(_.myInner)
