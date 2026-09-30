//> using options -language:experimental.specializedTraits

inline trait Container[T: Specialized](val elem: T)

def apply(f: Container[Int] => Unit, v: Container[Int]) =
    f(v)

def f(e: Container[Int]) =
    println(e.elem)

@main def Test =
    val v = new Container(10) {}
    apply(f, v)
    apply((e: Container[Int]) => println(e), v)
