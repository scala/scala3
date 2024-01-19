// nopos-error

import annotation.experimental

class Foo :
  final override def toString(): String = "Hello"

@experimental
@toString
class AFoo extends Foo //:
  //override def toString(): String = "Hello from macro"

@experimental
@main def run =
  println(new AFoo().toString)
