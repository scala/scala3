import language.future // sepchecks on
import caps.consume

def test1(@consume c: Object^, f: Object^ => Object^) =
  def cc: Object^ = c
  val x1 =
    { f(cc) } // ok
  val x2 =
    f(cc)  // ok
  val x3: Object^ =
    f(cc) // ok
  val x4: Object^ = // error
    { f(c) } // error

def test2(@consume c: Object^, f: Object^ ->{c} Object^) =
  def cc: Object^ = c
  val x1 =
    { f(cc) } // error // error
  val x4: Object^ =
    { f(c) } // error // error









