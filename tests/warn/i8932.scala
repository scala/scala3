sealed trait Foo[+A]
case class Bar[A]() extends Foo[A]

class Dummy extends Bar[Nothing] with Foo[String]

def bugReport[A](foo: Foo[A]): Foo[A] =
  foo match {
    case bar: Bar[A] => bar    // warn: unchecked
    case dummy: Dummy => ???
  }

def test = bugReport(new Dummy: Foo[String])
