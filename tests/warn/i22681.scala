
//> using options -Wunused:all

trait T:
  def t: Int

class C:
  def f: Runnable { def u: Int } = new Runnable with T:
    def run() = ()
    def g = 42 // warn effectively private member is unused
    def t = 42 // nowarn
    def u = 42 // nowarn because leaked by refinement
  val v: Runnable { def u: Int } = new Runnable:
    def run() = ()
    def u = 42 // nowarn because leaked by refinement
