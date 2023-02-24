//> using options -language:experimental.inlineTraits

class C extends B

@main def main =
    val x = C()
    println(x.foo)
    println(x.bar)
