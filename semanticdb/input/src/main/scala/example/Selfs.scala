package example

class B

class AC1 extends B { self =>
}

class AC2 extends B { self: B =>
}

abstract class AC3 extends B { self: B with Int =>
}

class AC4 extends B { a =>
}

class AC5 extends B { a : B =>
}

class AC6 extends B { this: B =>
}

abstract class AC7 { a: B =>
}
