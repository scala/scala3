def f(x: Int = 0): Int = 1
def f(x: Int*): Int = 2

@main def Test = println(f())
