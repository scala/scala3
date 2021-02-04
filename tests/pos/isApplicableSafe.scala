import reflect.ClassTag
import compiletime.uninitialized
// The same problems arise in real arrays.
class A {

  class Array[T]
  object Array {
    def apply[T: ClassTag](xs: T*): Array[T] = ???
    def apply(x: Int, xs: Int*): Array[Int] = ???
  }

  // Any of Array[List[Symbol]], List[Array[Symbol]], or List[List[Symbol]] compile.
  var xs: Array[Array[Symbol]] = uninitialized
  var ys: Array[Map[Symbol, Set[Symbol]]] = uninitialized

  //xs = Array(Array())
    // gives:
    //
    // isApplicableSafe.scala:15: error: type mismatch:
    // found   : A.this.Array[Nothing]
    // required: A.this.Array[Symbol]
    // xs = Array(Array())
    //
    // Here's the sequence of events that leads to this problem:
    //
    // 1. the outer Array.apply is overloaded, so we need to typecheck the inner one
    //    without an expected prototype
    //
    // 2. The inner Array.apply needs a ClassTag, so we need to instantiate
    //    its type variable, and the best instantiation is Nothing.
    //
    // To prevent this, we'd need to do several things:
    //
    // 1. Pass argument types lazily into the isApplicable call in resolveOverloaded,
    //    so that we can call constrainResult before any arguments are evaluated.
    //
    // 2. This is still not enough because the result type is initially an IgnoredProto.
    //    (because an implicit might have to be inserted around the call, so we cannot
    //     automatically assume that the call result is a subtype of the expected type).
    //    Hence, we need to somehow create a closure in constrainResult that does the
    //    comparison with the real expected result type "on demand".
    //
    // 3. When instantiating a type variable we need to categorize that some instantiations
    //    are suspicous (e.g. scalac avoids instantiating to Nothing). In these
    //    circumstances we should try to excute the delayed constrainResult closures
    //    in order to get a better instance type.
    //
    // Quite a lot of work. It's looking really complicated to fix this.


  ys = Array(Map(), Map())

  val zs = Array(Map())
}
