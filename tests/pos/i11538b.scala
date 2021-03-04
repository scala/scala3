package a:
  type Foo
  given foo: Foo = ???

import a.{Foo, given}
object test:
  inline def summonInlineFoo = scala.compiletime.summonInline[Foo]
  val summoned = summon[Foo]
  val summonedInline = summonInlineFoo
