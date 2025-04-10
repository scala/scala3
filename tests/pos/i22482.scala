//> using options -Yimplicit-as-given

def foo(implicit a: Int): Unit = ???
def bar()(implicit a: Int) : Unit = ???

def main() =
  foo(0)
  bar()(0)

class Foo(implicit hashA: Int):
  // added empty param list, works only for implicits
  def this(o: Int, h: Int) = this()(h)
