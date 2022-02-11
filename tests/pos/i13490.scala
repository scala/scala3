object MyApi {
  enum MyEnum(a: Int) {
    case A extends MyEnum(1)
  }
  case class Foo(a: MyEnum)
}

object Test {
  export MyApi.*
  import MyEnum.*
  Foo(MyEnum.A) match {
    case Foo(a) =>
      a match {
        case A =>
      }
  }
}
