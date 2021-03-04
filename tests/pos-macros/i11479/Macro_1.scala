trait Foo
given Foo: Foo with {}
inline def summonFoo(): Foo = scala.compiletime.summonInline[Foo]

package p:
  trait Bar
  given Bar: Bar with {}
  inline def summonBar(): Bar = scala.compiletime.summonInline[Bar]

