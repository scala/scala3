//> using options -Yimplicit-as-given

def foo(implicit a: Int): Unit = ???
def bar()(implicit a: Int) : Unit = ???

def main() =
  foo(0)
  bar()(0)
