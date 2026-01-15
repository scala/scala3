//> using options -Werror -deprecation -feature

object Foo {
  enum MyEnum {
    case Red
    case Blue(msg: String)
  }
  export MyEnum._
}

object Bar {
  type Blue = Foo.Blue
}

import Foo.*

def foo(a: MyEnum): Seq[Bar.Blue] = a match {
  case Red => Seq.empty
  case m: Foo.Blue => Seq(m)
}
