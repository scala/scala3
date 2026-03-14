// Test that return type qualifiers of function calls are used as assumptions.

def double(x: Int): {res: Int with res == x + x} = x + x

// The solver should use double's return type to prove double(3) == 6.
val d: {res: Int with res == 6} = double(3)

// Chained: the result of one qualified call used in another.
def inc(x: Int): {res: Int with res == x + 1} = x + 1
val v: {res: Int with res == 4} = inc(3)
val w: {res: Int with res == 5} = inc(inc(3))
