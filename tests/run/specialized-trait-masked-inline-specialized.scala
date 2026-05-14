//> using options -language:experimental.specializedTraits

inline trait A[T: Specialized]:
    def foo() = assert(Thread.currentThread.getStackTrace()(1).getClassName() == "A$impl$Int")

inline def myMethod1 = new A[Int]() {}
inline def myMethod2 = myMethod1
inline def MyMethod3 = myMethod2

@main def Test = 
    val v = myMethod1
    v.foo()
