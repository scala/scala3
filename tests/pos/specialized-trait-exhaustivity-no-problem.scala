//> using options -language:experimental.specializedTraits -Werror

sealed inline trait Foo[S: Specialized]
inline trait Bar extends Foo[Int]

def doMatch(x: Foo[Int]) = x match {
    case x: Bar => println("Hello World")
}
