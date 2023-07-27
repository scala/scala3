def test1[A, B]: Unit = {
  def f[T](x: T{ def *(y: Int): T }): T = ???
  def test = f[scala.collection.StringOps | String]("Hello")
  locally:
    val test1 : (scala.collection.StringOps | String) { def *(y: Int): (scala.collection.StringOps | String) } = ???
    val test2 : (scala.collection.StringOps | String) { def *(y: Int): (scala.collection.StringOps | String) } = test1

  locally:
    val test1 : (Int | String) { def foo(x: Int): Int } = ???
    val test2 : (Int | String) { def foo(x: Int): Int } = test1

  locally:
    val test1 : ((Int | String) & Any) { def foo(): Int } = ???
    val test2 : ((Int | String) & Any) { def foo(): Int } = test1

  locally:
    val test1 : Int { def foo(): Int } = ???
    val test2 : Int { def foo(): Int } = test1

  locally:
    val test1 : (Int | String) { def foo(): Int } = ???
    val test2 : (Int | String) & Any = test1

  locally:
    val test1 : (Int | B) { def *(y: Int): Int } = ???
    val test2 : (Int | B) { def *(y: Int): Int } = test1

  locally:
    val test1 : (Int | String) = ???
    val test2 : (Int | String) = test1

  type Foo = Int | String
  locally:
    val test1 : Foo { type T = Int } = ???
    val test2 : (Int | String) = test1
}

def test2: Unit = {
  import reflect.Selectable.reflectiveSelectable

  trait A[T](x: T{ def *(y: Int): T }):
    def f: T = x * 2

  class B extends A("Hello")
}
