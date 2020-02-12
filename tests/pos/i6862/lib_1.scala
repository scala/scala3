trait Ctx
inline def foo(): Unit = (using x: Ctx) => ()
def bar[T](b: Ctx ?=> Unit): Unit = ???
