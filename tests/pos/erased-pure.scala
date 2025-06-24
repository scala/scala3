import language.experimental.erasedDefinitions

inline def id[T](x: T) = x

class C()

def foo[T](erased x: T): Unit = ()

class Pair[A, B](x: A, y: B)


def Test =
  foo(C())
  foo(id(C()))
  foo(Pair(C(), C()))
  foo(Pair(C(), 22))
  foo(Pair(C(), "hello" + "world"))
  foo(id(Pair(id(C()), id("hello" + "world"))))





