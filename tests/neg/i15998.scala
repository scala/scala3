
given split: Conversion[Int, List[Int]] = ???

def foo[A, CC[B]](ring: CC[A]): Unit = ()

val _ = foo(1) // error


def bar[X](using x: X): X = x

val _ = bar  // error
