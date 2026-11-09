//> using options -language:experimental.specializedTraits

object x extends Bar[Int] // Fine because it extends Bar which is not sealed

def foo(x: Foo[Int]) = println("hello world") // Generates $sp$ trait but actually fine

@main def main = ???
    val z = new Foo[Int]() {} // error: Extending sealed trait
