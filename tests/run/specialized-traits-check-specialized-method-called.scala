//> using options -language:experimental.specializedTraits

// Check we actually call the specialized method that we should be calling
// We can't easily check the return type, but we can check that we are calling a method
// on Foo$impl$Int$ directly, with no bridge methods in between (this means it's the correct
// method without boxing / unboxing).

inline trait Foo[T: Specialized](x: T):
  def foo = 
    val stackTrace = Thread.currentThread.getStackTrace()
    
    // No bridge methods; we call foo directly
    assert(stackTrace.toList.tail.takeWhile(call => call.getMethodName().startsWith("foo")).length == 1)

    // We call this method on the correct impl class
    assert(Thread.currentThread.getStackTrace()(1).getClassName() == "Foo$impl$Int$")
    x

def f(b: Foo[Int]) = 
    37 + b.foo
 
object Test:
  def main(args: Array[String]): Unit = {
    val x = new Foo[Int](42) {}
    f(x)
  }
