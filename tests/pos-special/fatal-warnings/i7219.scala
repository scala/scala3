object Foo {
  enum MyEnum {
    case Red
    case Blue(msg: String)
  }
  export MyEnum._
}

object Bar {
  type Blue = Foo.Blue
  
  // Related Issue -- my expectation is that
  // `export Foo.Blue` should be equivalent to
  // `type Blue = Foo.Blue`, but it's not:
  
  // export Foo.Blue // Uncommenting this (and commenting `type Blue = ...`) results in compile error
}

import Foo._

def foo(a: MyEnum): Seq[Bar.Blue] = a match {
  case Red => Seq.empty
  case m: Foo.Blue => Seq(m)
}
