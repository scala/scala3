trait Foo
trait Bar

given (foo: Foo = new {}) => Bar()

def Test = summon[Bar]
