implicit class Ops[A](val a: String) extends AnyVal {
  def foo(e: => String): Unit = ()
}

def bar(e1: => String): Unit = (new Ops("")).foo(e1)
def baz(e2: => String): Unit = "".foo(e2)

@main def Test = baz("")