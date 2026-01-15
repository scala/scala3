//> using options -feature -Werror -preview

import Conversion.into

case class Foo(x: Int)

given Conversion[Int, Foo] = Foo(_)

def takeFoo(f: into[Foo]) = f
inline def inlineTakeFoo(f: into[Foo]) = f
inline def takeInlineFoo(inline f: into[Foo]) = f

def test =
  val f1 = takeFoo(1)
  val f2 = inlineTakeFoo(1)
  val f3 = takeInlineFoo(1)
