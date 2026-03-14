// Test that qualified types can reference the enclosing method (self-reference).
// This used to cause a cyclic reference error because computing the method's
// signature required typing the qualifier, which referenced the method itself.
def selfRef(n: Int): {res: Int with res == selfRef(n)} = selfRef(n)
