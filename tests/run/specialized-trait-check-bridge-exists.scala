// scalajs: --skip
// (getName() reflection is not supported in ScalaJS)

//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized]:
    def foo(x: Foo[T]): Foo[T]

class Bar extends Foo[Int]:
    override def foo(x: Foo[Int]): Foo[Int] = x

@main def Test =
    val x = Bar()
    val fooMethods = classOf[Bar].getDeclaredMethods.filter(_.getName == "foo")
    assert(fooMethods.length == 2, s"expected 2 `foo` methods on Bar (real + bridge), found ${fooMethods.length}: ${fooMethods.mkString(", ")}")

