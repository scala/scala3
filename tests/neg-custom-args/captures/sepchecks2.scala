import language.future // sepchecks on

def foo(xs: List[() => Unit], y: Object^) = ???

def Test(c: Object^) =
  val xs: List[() => Unit] = (() => println(c)) :: Nil
  println(c) // error

def Test2(c: Object^) =
  foo((() => println(c)) :: Nil, c) // error
