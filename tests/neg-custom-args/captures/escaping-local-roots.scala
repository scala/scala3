import language.experimental.captureChecking

trait IO:
  def use: Unit

object test1 {
  def foo(c: IO^): IO^{cap[foo]} = c  // error
}

def test2 = {
  def foo(c: IO^) =  // error
    def bar: IO^{cap[foo]} = c
    bar
}

object test3 {
  def foo(c: IO^): IO^{cap[bar]} = ???  // error
  def bar(c: IO^): IO^{cap[foo]} = ???  // error
}
