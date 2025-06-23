import scala.language.experimental.erasedDefinitions

type X
inline def x: X = compiletime.erasedValue

def foo(using erased X): Unit = ()

def test: Unit = foo(using x)
