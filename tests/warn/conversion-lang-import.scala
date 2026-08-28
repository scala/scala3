import Conversion.into

given [A] => Conversion[A, Option[A]] = Some(_)

def foo(x: Option[String]) = ()
def bar(x: into[Option[String]]) = ()

def test =
  foo("abc")  // warn
  bar("abc")  // ok
