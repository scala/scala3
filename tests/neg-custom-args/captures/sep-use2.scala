
import caps.consume

def test1(@consume c: Object^, f: (x: Object^) => Object^) =
  def cc: Object^ = c  // error
  val x1 =
    { f(cc) } // ok
  val x2 =
    f(cc)  // ok
  val x3: Object^ =
    f(cc) // ok
  val x4: Object^ = // error
    { f(c) } // error

def test2(@consume c: Object^, f: (x: Object^) ->{c} Object^) =
  def cc: Object^ = c // error
  val x1 =
    { f(cc) } // error // error
  val x4: Object^ = // ^ hides just c, since the Object^ in the result of `f` is existential
    { f(c) } // error // error

def test3(@consume c: Object^, f: Object^ ->{c} Object^) =
  val x4: Object^ = // ^ hides c and f*
    { f(c) } // error











