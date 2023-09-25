
class Err extends Exception: // error
  self: Err^ =>

def test(c: Any^) =
  class Err2 extends Exception:
    val x = c  // error
  class Err3(c: Any^) extends Exception // error

class Err4(c: Any^) extends AnyVal // was error, now ok


