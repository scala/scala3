//> using options -experimental -language:experimental.erasedDefinitions

class Foo(erased x: Int) extends AnyVal // error

class Bar(x: Int)(y: Int) extends AnyVal // error
