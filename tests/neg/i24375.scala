def foo(f: (x: Int) ?=> Int) = f(using 0)
@main def Main = println(foo(x))