type Foo[A] = A

def foo[A <: Foo[A]]: Unit = () // error // error
