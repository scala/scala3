trait A
trait B
trait C

def fancy given (a: A, b: B, c: C) = "Fancy!"
def foo(implicit a: A, b: B, c: C) = "foo"
