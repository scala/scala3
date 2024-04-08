import deriving.Mirror
import compiletime.summonInline

type DoesNotReduce[T] = T match
  case String => Any

type DoesNotReduce2[T] <: T = T match
  case String => T

class Foo
@main def Test: Unit =
  summonInline[Mirror.Of[DoesNotReduce[Option[Int]]]] // error
  summonInline[Mirror.Of[DoesNotReduce2[Option[Int]]]] // error
