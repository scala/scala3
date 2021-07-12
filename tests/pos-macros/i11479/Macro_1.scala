trait Foo
given Foo: Foo()
inline def summonFoo(): Foo = scala.compiletime.summonInline[Foo]

package p:
  trait Bar
  given Bar: Bar()
  inline def summonBar(): Bar = scala.compiletime.summonInline[Bar]

