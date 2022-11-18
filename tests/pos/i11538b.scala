package a:
  type Foo
  given foo: Foo = ???

import a.{Foo, given}
object test:
  transparent inline def summonInlineFoo = scala.compiletime.summonInline[Foo]
  val summoned = summon[Foo]
  val summonedInline = summonInlineFoo
