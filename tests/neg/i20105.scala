import scala.annotation.tailrec
@tailrec
def foo(): Unit = // error
  def bar(): Unit =
    if (???)
      foo()
    else
      bar()
  bar()