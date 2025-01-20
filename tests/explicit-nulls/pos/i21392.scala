//> using options -language:strictEquality

import scala.collection.LinearSeq

def foo[T](a: LinearSeq[T]) = a match
  case Nil => -1
  case head +: tail => head

enum Foo derives CanEqual:
  case Bar
  case Baz(x: String)


def foo(a: Foo) = a match
  case Foo.Bar => -1
  case _ => 0