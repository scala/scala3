package covtest

object PolyExt:
  extension (s: String)
    def foo[A](x: A): A = x

  extension [A](i: Int)
    def get(x: A): A = x

  "str".foo(0) // ({foo("str")}[type])(0) i.e. Apply(TypeApply( Apply(foo, "str"), type ), List(0))
  123.get(0) // {(get[type])(123)}(0) i.e. Apply(Apply(TypeApply(...), List(123)), List(0))
