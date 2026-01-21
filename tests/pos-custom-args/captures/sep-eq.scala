import caps.Mutable
import caps.any


extension (x: Object^)
  infix def eql (y: Object^{x, any}): Boolean = x eq y

def eql1(x: Object^, y: Object^{x, any}): Boolean = x eql y
def eql2(x: Object^)(y: Object^{x, any}): Boolean = x eql y

class LLI extends Object:
  this: LLI^ =>

  val f: Object^ = ???

  def foo =
    def these = f
    val eq0 = these eql these
    val eq1 = eql2(f)(f)
    val eq2 = eql2(these)(these)
