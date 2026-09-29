//> using options -language:experimental.specializedTraits

inline trait A[T: Specialized]:
    def foo() = assert(Thread.currentThread.getStackTrace()(1).getClassName() == "A$impl$scala$Int")

inline def myMethod1 = new A[Int]() {}
inline def myMethod2 = myMethod1
inline def myMethod3 = myMethod2

@main def Test = 
    val v = myMethod3
    v.foo()
