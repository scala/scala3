//> using options -language:experimental.specializedTraits

package package2

inline trait A[T: Specialized]:
    def foo(x: T) = "Package 2!"
    def bar = Thread.currentThread.getStackTrace()(1).getClassName()
