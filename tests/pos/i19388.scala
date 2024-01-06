import language.implicitConversions
class Foo[+T]

implicit inline def conv[T, R](from: R): Foo[T] =
  compiletime.summonInline[T =:= Int]
  new Foo[T]

val f: Foo[Int] = "o"