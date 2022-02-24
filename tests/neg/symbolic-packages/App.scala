package example

class Example:

  def foo: symbolic_>> = ??? // error: No type found
  def bar: _root_.symbolic_>> = ??? // ok - will "mock superclass"
