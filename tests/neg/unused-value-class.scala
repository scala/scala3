
class Foo(unused x: Int) extends AnyVal // error

class Bar(x: Int)(y: Int) extends AnyVal // error
