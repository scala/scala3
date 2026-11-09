//> using options -language:experimental.specializedTraits

inline trait A[T: Specialized]:
    def foo() = assert(Thread.currentThread.getStackTrace()(1).getClassName() == "A$impl$scala$Int")

inline def bar = new A[Int]() {}
