
class Err extends Exception:
  self: Err^ => // error

def test(c: Object^) =
  class Err2 extends Exception:
    val x = c  // error
  class Err3(c: Object^) extends Exception // error

class Err4(c: Object^) extends AnyVal // was error, now ok


