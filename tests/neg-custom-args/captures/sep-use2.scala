import language.future // sepchecks on

def test1(c: Object^, f: Object^ => Object^) =
  def cc: Object^ = c
  val x1 =
    { f(cc) } // ok
  val x2 =
    f(cc)  // ok
  val x3: Object^ =
    f(cc) // ok
  val x4: Object^ =
    { f(c) } // error

def test2(c: Object^, f: Object^ ->{c} Object^) =
  def cc: Object^ = c
  val x1 =
    { f(cc) } // error // error
  val x4: Object^ =
    { f(c) } // error // error









