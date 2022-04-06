trait Foo
class A(x: Int)
class Bar extends Foo with A(10) // error
