def f(x: Char)(someParam: String): String = "a"
def f(x: Char)(min: Boolean, max: Int = 4): String = "b"

@main def Test() = f('c')(2) // error
