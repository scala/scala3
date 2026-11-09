//> using options -language:experimental.specializedTraits

class Bar extends Foo[Boolean](true)

@main def Test =
    val x = new Foo[Int](100) {}
    val y  = Bar()

    assert(Methods.foo(x) == 100)
    assert(Methods.bar(y) == true)
