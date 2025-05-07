
//> using options -Wunused:all

trait T:
  def t: Int

class C:
  def f: Runnable { def u: Int } = new Runnable with T:
    private def v = 42 // avoid g judged too trivial to warn
    def run() = ()
    def g = v // warn effectively private member is unused
    def t = v // nowarn
    def u = v // nowarn because leaked by refinement
  val v: Runnable { def u: Int } = new Runnable:
    private def v = 42 // avoid g judged too trivial to warn
    def run() = ()
    def u = v // nowarn because leaked by refinement
