trait A
trait B
trait C

def fancy(using a: A, b: B, c: C) = "Fancy!"
def foo(implicit a: A, b: B, c: C) = "foo"

given A with B with {}

given ops: A with B with {}