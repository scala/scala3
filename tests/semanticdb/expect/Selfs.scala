package selfs

class B

class C1 extends B { self =>
}

class C2 extends B { self: B =>
}

class C3 extends B { self: B with C1 =>
}

class C6 extends B { this: B =>
}
