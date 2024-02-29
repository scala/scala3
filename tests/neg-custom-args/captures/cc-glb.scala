import language.experimental.captureChecking
trait Cap
trait Foo[+T]

def magic[T](io: Cap^, x: Foo[T]^{io}): Foo[T]^{} =
  val x1: (Foo[T]^) & Foo[Any]^{io} = x
  val x2: Foo[T] = x1  // error
  x2  // boom, an impure value becomes pure
