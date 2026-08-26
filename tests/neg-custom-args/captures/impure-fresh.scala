import caps.fresh

class A

val x: () => A^{fresh} = ???
val y: () -> A^{fresh} = x  // error

