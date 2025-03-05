
//> using options -Wunused:all

trait T:
  def t: Int

class C:
  def f: Runnable { def u: Int } = new Runnable with T:
    private def v = 42 // avoid g judged too trivial to warn
    def run() = ()
    def g = v // warn effectively private member is unused
    def t = v // nowarn
    def u = v // warn despite structural type (TODO work around the limitation, at least for this example)
