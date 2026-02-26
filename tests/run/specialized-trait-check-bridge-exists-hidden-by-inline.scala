// scalajs: --skip
// (getName() reflection is not supported in ScalaJS)

//> using options -language:experimental.specializedTraits
inline trait Trait[T: Specialized]:
    def do_something() = println("Good morning")
  
inline trait A[T: Specialized]:
    def foo(x: Trait[T]): Unit

inline trait B extends A[Int]:
    override def foo(x: Trait[Int]): Unit = x.do_something()

class C extends B // Should have a bridge for the specialized foo

@main def Test = 
    val fooMethods = classOf[C].getDeclaredMethods.filter(_.getName == "foo")
    assert(fooMethods.length == 2, s"expected 2 `foo` methods on Bar (real + bridge), found ${fooMethods.length}: ${fooMethods.mkString(", ")}")

