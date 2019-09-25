import compiletime.erasedValue

sealed trait Foo
trait A extends Foo
trait B[H] extends Foo

inline given f[T <: Foo]: T = inline erasedValue[T] match
  case _: A => new A{}.asInstanceOf[T]
  case _: B[a] => summon[a].asInstanceOf[T]

@main def Test = println(f[B[Int]])
