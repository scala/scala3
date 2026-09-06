//> using options -language:experimental.specializedTraits

sealed inline trait Foo[T: Specialized]
inline trait Bar[T: Specialized] extends Foo[T]

def foo(x: Foo[Int]) = x match {
    case y: Bar[Int] => println("ok boomer")
}
