//> using options -source 3.11
import Conversion.into

given [A] => Conversion[A, Option[A]] = Some(_)

def foo(x: Option[String]) = ()
def bar(x: into[Option[String]]) = ()

def test =
  foo("abc")  // error
  bar("abc")  // ok
