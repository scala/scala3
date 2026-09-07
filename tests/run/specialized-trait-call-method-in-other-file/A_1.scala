//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized](val x: T):
    def foo = Thread.currentThread.getStackTrace()(1).getClassName()

object Methods:
   def foo(y: Foo[Int]) =
      assert(y.foo == "Foo$impl$scala$Int")
      y.x

   def bar(y: Foo[Boolean]) =
      assert(y.foo == "Bar")
      y.x
