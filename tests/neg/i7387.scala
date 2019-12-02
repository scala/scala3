type E = A.B { type C = D } // error: Not found: A
def g = summon[E] // error: no implicit argument of type E
