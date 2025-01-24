import language.future // sepchecks on


def foo(xs: List[() => Unit], y: Object^) = ???

def bar(x: (Object^, Object^)): Unit = ???

def Test(c: Object^) =
  val xs: List[() => Unit] = (() => println(c)) :: Nil
  println(c) // error

def Test2(c: Object^, d: Object^): Unit =
  foo((() => println(c)) :: Nil, c) // error
  val x1: (Object^, Object^) = (c, c) // error
  val x2: (Object^, Object^{d}) = (d, d) // error

def Test3(c: Object^, d: Object^) =
  val x: (Object^, Object^) = (c, d) // ok

def Test4(c: Object^, d: Object^) =
  val x: (Object^, Object^{c}) = (d, c) // ok

def Test5(c: Object^, d: Object^): Unit =
  bar((c, d)) // ok

def Test6(c: Object^, d: Object^): Unit =
  bar((c, c)) // error

