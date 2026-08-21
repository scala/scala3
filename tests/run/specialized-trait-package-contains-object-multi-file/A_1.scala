//> using options -language:experimental.specializedTraits
package package1 {
    object Outer:
        object Inner:
            inline trait A[T: Specialized]:
                def foo(x: T) = "Package 1!"
                def bar = Thread.currentThread.getStackTrace()(1).getClassName()
    class B extends Outer.Inner.A[Int]
    class C extends Outer.Inner.A[String]
}

