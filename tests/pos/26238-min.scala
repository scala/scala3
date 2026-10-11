trait Thing[T]
trait G

object Outer:

  def thing: Thing[Int] = ???

  extension[T1](t: Thing[T1])(using g: G = new G { })
    def map[T2](m: T1 => T2): Thing[T2] = ???
    def flatMap[T2](m: T1 => Thing[T2]): Thing[T2] = ???

  // `y` requires a `flatMap`, and that one requires getting the default value for the `g` argument,
  // and getting that default value requires the `t` argument (since default values can depend on previous args),
  // which means we end up with a block;
  // and since the whole thing is generic, we must apply a generic type argument to that block
  for
    x <- thing
    y <- thing
  yield
    1
