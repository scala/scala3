// Test forward references handled with flow typing
// Currently, the flow typing will not be applied to definitions forwardly referred.
class Foo {

  def foo(): Unit = {
    val x: String|Null = ???
    if (x == null) return ()
    implicit val y: Int = x.length
  }

  // This case is not valid but the foo() above is valid, because
  // non-lazy value definitions exist between forward references.
  // Since y is referred (by z) before definition, flow typing is not used here.
  // Only the typing error is shown because reference check is after typing.
  def fr1(): Unit = {
    val z = implicitly[Int]
    val x: String|Null = ???
    if (x == null) return ()
    implicit val y: Int = x.length // error: x: String|Null inferred
  }

  // Since z is referred before definition, flow typing is not used here.
  def fr2(): Unit = {
    val x: String|Null = ???
    if (x == null) return ()
    def y = z
    def z = x.length // error: x: String|Null inferred
  }
}
