trait Foo
given Foo: Foo with {}
inline def summonFoo(): Foo = scala.compiletime.summonInline[Foo]
