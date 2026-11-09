//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits -Werror

// nopos-error: (warning) non-exhaustive pattern match

sealed inline trait Foo[S: Specialized]
inline trait Bar extends Foo[Int]

def doMatch(x: Foo[Int]) = x match {
    case x: Bar => println("Hello World")
}

def main = 
    val x = new Foo[Int]() {}
    doMatch(x)


