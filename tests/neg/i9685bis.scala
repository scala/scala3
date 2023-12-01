// scalac: -source future

package foo

import scala.language.implicitConversions

class Foo

object Foo:

  inline implicit def toFoo(x: Int): Foo = Foo()

class Bar

object Bar:
  inline given Conversion[Int, Bar] with
    def apply(x: Int): Bar = Bar()

class Baz

object Baz:
  transparent inline implicit def toBaz(x: Int): Baz = Baz()

object Usage:
  1.asdf // error
