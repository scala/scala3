trait Foo
trait Bar

given (using foo: Foo = new {}): Bar with {}

def Test = summon[Bar]
