implicit class Ops[A](val a: String) extends AnyVal {
  def foo(e: => String): Unit = ()
}

def bar(e: => String): Unit = (new Ops("")).foo(e)
def baz(e: => String): Unit = "".foo(e)

@main def Test = baz("")