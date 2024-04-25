class A
class B
given a1: A = ???
given a2: A = ???
def g(using a: A): B = ???

// In this case, the ambiguous given instance is directly the argument of
// `summon`. This is a direct ambiguous implicit, thus we report the error
// directly. See `given-ambiguous-1` for a nested ambiguous implicit error.
def f: Unit = g // error: Ambiguous given instances
