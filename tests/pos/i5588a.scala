object TestMain {
  def main(args: Array[String]): Unit = {
    testExtensionProperty()
  }

  case class Foo(var foo: String)

  object fooExt {
    // define `Foo`-extension property with getter and setter delegating to `Foo.foo`
    extension (thisFoo: Foo) def fooExt: String = thisFoo.foo
    extension (thisFoo: Foo) def fooExt_= (value: String): Unit = { thisFoo.foo = value }
  }

  def testExtensionProperty(): Unit = {
    import fooExt.*
    val foo = Foo("initVal")
    assert(foo.fooExt == "initVal")
    foo.fooExt = "updatedVal"
    assert(foo.foo == "updatedVal")
    assert(foo.fooExt == "updatedVal")
  }
}
