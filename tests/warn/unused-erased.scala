//> using options -Wunused:all -language:experimental.erasedDefinitions

trait Ev:
  def foo: Int

def answer: Int = 42

def f(using erased ev: Ev): Int = answer     // no warn, erased using param is unused by design

def g(erased x: Int): Int = answer           // no warn, erased explicit param is unused by design

def h(using ev: Ev): Int = answer            // warn, non-erased using param is still checked

def i(x: Int): Int = answer                  // warn, non-erased explicit param is still checked
