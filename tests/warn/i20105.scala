import scala.annotation.tailrec
@tailrec
def foo(): Unit =
  def bar(): Unit =
    if (???)
      foo() // warn
    else
      bar()
  bar()
  foo()