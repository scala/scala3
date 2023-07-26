import scala.language.implicitConversions

def foo(a: Int): Int = ???
def bar(f: () => Int): Int = ???

given f: Conversion[Int => Int, () => Int] = ???

def test1: Int = bar(foo) // implicit conversion applied to foo
def test2: Int = bar(f(foo))
