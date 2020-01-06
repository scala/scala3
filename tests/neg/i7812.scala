def f(): Any = ???
var f: (UndefinedA & UndefinedB) { val x: Int } = ??? // error // error
val a = f // error