import language.experimental.erasedDefinitions

object Foo:
    private erased val evidence: Boolean = true
    inline def foo = Bar.bar(evidence)

object Bar:
    def bar(erased x: Boolean) = println("Hello World")

def main = 
    Foo.foo
