package covtest

object PolyExt:
  extension (s: String)
    def foo[A](x: A): A = x

  extension [A](i: Int)
    def get(x: A): A = x
    def tap[U](f: Int => U): Int = ???

  "str".foo(0) // ({foo("str")}[type])(0) i.e. Apply(TypeApply( Apply(foo, "str"), type ), List(0))
  123.get(0) // {(get[type])(123)}(0) i.e. Apply(Apply(TypeApply(...), List(123)), List(0))

  def foo: Int = 42
  def bar: Int = foo.tap(println)
