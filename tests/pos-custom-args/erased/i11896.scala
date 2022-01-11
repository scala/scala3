import scala.language.experimental.erasedDefinitions

type X
erased def x: X

def foo(using erased X): Unit = ()

def test: Unit = foo(using x)
