//> using options -language:experimental.specializedTraits
trait Illegal:
    def foo: Specialized[Int] // error: Specialized may only be used as a context bound
    
    def bar(x: Specialized[Int]): Int // error: Specialized may only be used as a context bound
    
    class Foo(x: Specialized[Int]) // error: Specialized may only be used as a context bound
    
    class Bar(val x: Specialized[Int]) // error: Specialized may only be used as a context bound

    val y: Specialized[Char] // error: Specialized may only be used as a context bound

    val v = Specialized.apply[Int] // error: Specialized may only be used as a context bound

    def z = 
        println(Specialized.apply[Float]) // error: Specialized may only be used as a context bound
    
    def a = Specialized.apply // error: Specialized may only be used as a context bound

    type V = Specialized // error: Specialized may only be used as a context bound

    type W = Specialized[Int] // error: Specialized may only be used as a context bound
    
    val b = Specialized // error: Specialized may only be used as a context bound
