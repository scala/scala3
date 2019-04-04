// Test that Null is a subtype of Any, but not of AnyRef.

class Foo {

  val x1: Any = null 
  val x2: AnyRef = null // error
  val x3: AnyRef|Null = null
  val x4: Any|Null = null // Any|Null == Any

  {
    def bar(a: Any): Unit = ()
    val s: String|Null = ???
    bar(s)
    val s2: Int|Null = ???
    bar(s2)
  }
}
