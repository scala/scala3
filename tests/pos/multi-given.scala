trait A
trait B
trait C

def fancy(using a: A, b: B, c: C) = "Fancy!"
def foo(implicit a: A, b: B, c: C) = "foo"

given A, B {}

given A with B as ops {}