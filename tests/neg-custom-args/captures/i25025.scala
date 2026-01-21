import language.experimental.captureChecking

def foo[c^](c: AnyRef^): AnyRef^{c} = ??? // error
def bar[c^](c: AnyRef^): Int = 0 // error
def baz(c: AnyRef^)[c^](d: AnyRef^): AnyRef^{c} = d  // error
def bro(c: AnyRef^)[c^](d: AnyRef^): AnyRef^{c} = c  // error

trait Foo[c^,d^]: // error
  val c: AnyRef^
  def meth(c: AnyRef^): AnyRef^{c}
  def meth2: AnyRef^{d}

trait Bar:
  type c^ // error
  def c: AnyRef^

trait Baz[c^]: // error
  var c: AnyRef^
