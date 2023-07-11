
def foo(using xs: Int*) = xs // error
def foo5(using d: Int, xs: Int*) = xs // error
def foo2(implicit xs: Int*) = xs // error
def foo3(u: Int)(using d: Int, xs: Int*) = xs // error
def foo4(u: Int)(implicit d: Int, xs: Int*) = xs // error

extension (i: Int)
  def bar(using xs: Float*) = ??? // error
  def bar2(using d: Boolean, xs: Float*) = ??? // error
