//> using options -language:experimental.specializedTraits

package package1

inline trait A[T: Specialized]:
    def foo(x: T) = "Package 1!"
    def bar = Thread.currentThread.getStackTrace()(1).getClassName()
    