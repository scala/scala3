trait Foo
class Bar

given (using foo: Foo = new {}): Bar()

def Test = summon[Bar]
