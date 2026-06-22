import scala.language.experimental.erasedDefinitions

type X
inline def x: X = caps.unsafe.unsafeErasedValue

def foo(using erased x: X): Unit = ()

def test: Unit = foo(using x)
