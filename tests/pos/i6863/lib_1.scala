trait Ctx
inline def foo(): Unit = (using x: Ctx) => ()
