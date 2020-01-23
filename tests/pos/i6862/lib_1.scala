trait Ctx
inline def foo(): Unit = (x: Ctx) ?=> ()
def bar[T](b: Ctx ?=> Unit): Unit = ???
