import language.experimental.erasedDefinitions
import caps.unsafe.unsafeErasedValue

inline def id[T](x: T) = x

class C()

def foo[T](erased x: T): Unit = ()

class Pair[A, B](x: A, y: B)

case class Pair2[A, B](x: A, y: B)

def Test =
  foo(C())
  foo(id(C()))
  foo(Pair(C(), C()))
  foo(Pair(C(), 22))
  foo(Pair(C(), "hello" + "world"))
  foo(id(Pair(id(C()), id("hello" + "world"))))

  //erased val x1 = Pair(unsafeErasedValue[Int], unsafeErasedValue[String])
  //erased val x2 = Pair2(unsafeErasedValue[Int], unsafeErasedValue[String])
  erased val x3 = Tuple2(unsafeErasedValue[Int], unsafeErasedValue[String])


