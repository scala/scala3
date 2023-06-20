def foo: Unit =
  val _root_  = "abc"
  println(_root_.length)  // error
  println(_root_)  // error

def bar: Unit =
  _root_  // error
  _root_.scala  // error
  println(_root_)  // error
  println(_root_.scala)  // error
