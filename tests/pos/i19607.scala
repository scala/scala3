trait Foo
trait Bar[T]

type MatchType[T] = T match
  case Bar[?] => Nothing
  case _ => T

object Test:
  def foo(b: Bar[? >: Foo]): Unit =
    summon[MatchType[b.type] =:= Nothing]
    summon[MatchType[Bar[? >: Foo]] =:= Nothing]
end Test
