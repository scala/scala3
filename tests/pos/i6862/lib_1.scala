trait Ctx
inline def foo(): Unit = given (x: Ctx) => ()
def bar[T](b: given Ctx => Unit): Unit = ???
