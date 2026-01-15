import language.experimental.captureChecking
import language.experimental.separationChecking

def example(a: AnyRef^, b: AnyRef^, c: AnyRef^) =
    abstract class Root:
        this: Root^{a,b} =>

    trait A:
        self: A^{a} =>

    trait B:
        self: B^{b} =>

    trait C:
        self: C^{c} =>

    // This should error because Sub captures c through field f,
    // but parent Root only allows {a, b} in its self-type. (#24676)
    class Sub extends Root: // error
        val f: AnyRef^ = c // error

    class Sub2 extends Root, A:
        val f: AnyRef^{a} = a // ok

    class Sub3 extends Root, A:
        self: Sub3^{a} =>
        val f: AnyRef^{a} = a // ok

    class Sub4 extends Root, A: // error
        val f: AnyRef^ = c // error

    class Sub5 extends A, B, C:
        self: Sub5^{} =>
        val f: AnyRef^{} = null // ok

    class Sub6 extends A, B, C: // error
        self: Sub6^{a, b, c} =>

    class Sub7 extends Root, A: // error
        self: Sub7^{a, b} =>

