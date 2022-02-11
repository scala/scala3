object MyTypes:
  enum MyEnum:
    case Foo
    case Bar

object MyApi:
  export MyTypes.*

object MyUse:
  import MyApi.MyEnum.Foo
  def foo = Foo

@main def Test = assert(MyUse.foo.toString == "Foo")
