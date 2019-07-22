trait Ctx
inline def foo(): Unit = given (x: Ctx) => ()
