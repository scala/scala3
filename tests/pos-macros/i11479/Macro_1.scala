trait Foo
given Foo: Foo()
transparent inline def summonFoo(): Foo = scala.compiletime.summonInline[Foo] : Foo

package p:
  trait Bar
  given Bar: Bar()
  transparent inline def summonBar(): Bar = scala.compiletime.summonInline[Bar] : Bar

