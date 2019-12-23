trait Foo[A]
class A {
    def F[x : Foo]() = ???

    Int match {
        case Int | F => ()  // error
    }
}