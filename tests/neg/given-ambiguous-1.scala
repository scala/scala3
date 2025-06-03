class A
class B
given a1: A = ???
given a2: A = ???
given (using a: A): B = ???

// In this case, the ambiguous given instance is not directly the argument of
// `summon`; it is the argument of `given_B` which is needed for the argument of
// `summon`. This is a nested ambiguous implicit, thus we report an error in
// the style "I found ... but". See `given-ambiguous-2` for a direct ambiguous
// implicit error.
def f: Unit = summon[B] // error: Ambiguous given instances
