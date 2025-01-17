trait A
trait B
trait C

def fancy(using a: A, b: B, c: C) = "Fancy!"
def foo(implicit a: A, b: B, c: C) = "foo"

given A() with B

given ops: A() with B()

given ops2: A(), B
