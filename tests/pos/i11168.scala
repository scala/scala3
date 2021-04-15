trait Foo
given foo: Foo()

extension (using Foo)(x: Any)
  def foo1[A] = ???

extension (x: Any)(using Foo)
  def foo2[A] = ???

implicit class LeadingFooOps(using Foo)(x: Any) {
  def foo3[A] = ???
}

implicit class TrailingFooOps(x: Any)(using Foo) {
  def foo4[A] = ???
}

def a1 = "".foo1[Any]
def a2: Any = "".foo2[Any]
def a3 = "".foo3[Any]
def a4 = "".foo4[Any]

def b2 = "".foo2(using foo)[Any]

def c1 = foo1("")[Any]
def c2 = foo2("")[Any]

def d1 = foo1(using foo)("")[Any]
def d2 = foo2("")(using foo)[Any]