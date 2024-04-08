import deriving.Mirror
import compiletime.summonInline

type DoesNotReduce[T] = T match
  case String => Any

class Foo
@main def Test: Unit =
  summonInline[Mirror.Of[DoesNotReduce[Option[Int]]]] // error
