//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits
inline trait Trait[S: Specialized]

def foo(x: Trait[?], y: Trait[? <: Long]) = // error: Wildcard types may not be substituted for Specialized type parameters. // error: Wildcard types may not be substituted for Specialized type parameters.
    println("HELLO WORLD")

def bar(x: Trait[_]) =                      // error: Wildcard types may not be substituted for Specialized type parameters.
    println("HELLO WORLD")

def baz(x: Trait[? >: Long]) =              // error: Wildcard types may not be substituted for Specialized type parameters.
    println("HELLO WORLD")

def foo2(x: Trait[? >: Long <: Long]) =     // error: Wildcard types may not be substituted for Specialized type parameters.
    println("HELLO WORLD")

def main = 
    foo(new Trait[Int]() {}, new Trait[Long]() {})
