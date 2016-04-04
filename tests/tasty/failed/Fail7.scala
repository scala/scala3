//Problem with unpickling
//error: class Test needs to be abstract, since <accessor> var in_=: (x$1: Int)Unit is not defined
//  (Note that an abstract var requires a setter in addition to the getter)
//  one error found
class Fail7(var in: Int)

/*
//This code fails while compiling with -Ycheck:all
//Exception in thread "main" java.lang.AssertionError: assertion failed: method in_$eq is both Deferred and Private
abstract class Fail7(private var in: Int, out: Double) {
  val interpreter = 5
}
*/