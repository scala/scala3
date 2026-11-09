//> using options -language:experimental.specializedTraits

// Lib1 and Lib2 both specialize A, can we share their specializations?
// At the moment we don't. Do we still avoid symbol collisions?

inline trait A[T: Specialized]:
    def foo = Thread.currentThread.getStackTrace()(1).getClassName()
