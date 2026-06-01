//> using options -language:experimental.specializedTraits -Werror

// nopos-error: (warning) non-exhaustive pattern match

sealed inline trait Foo[S: Specialized]
sealed inline trait Baz[S: Specialized] extends Foo[S]
inline trait Bar extends Foo[Int]

def doMatch(x: Foo[Int]) = x match {
    case x: Baz[_] => println("Hello World")
}


