inline trait Foo:
    private def foo = 10

class A extends Foo:
    override def foo = 11 // error: method foo overrides nothing
