//> using options -experimental

import scala.quoted.*

sealed trait Foo
class Bar extends Foo
object CustomCodecs {
  given named: Codec[Bar] = new Codec[Bar] { def print(): Unit = println("bar codec")}
  given Codec[Foo] = new Codec[Foo] { def print(): Unit = println("foo codec") }
}

@main def Test =
  Codec.derivedWithDeps[Bar](CustomCodecs).print()
  Codec.derivedWithDepsWithNamedOmitted[Bar](CustomCodecs).print()
