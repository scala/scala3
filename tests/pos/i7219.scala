class Foo {
  object MyEnum {
    class Blue
  }
  export MyEnum._

  val a: MyEnum.Blue = ???
  a : Blue   // ok
}
