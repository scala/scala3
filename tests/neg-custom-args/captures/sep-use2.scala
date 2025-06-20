
import caps.consume

def test1(@consume c: Object^, f: (x: Object^) => Object^) =
  def cc: Object^ = c  // error
  val x1 =
    { f(cc) } // ok
  val x2 =
    f(cc)  // ok
  val x3: Object^ =
    f(cc) // ok
  val x4: Object^ =
    { f(c) } // error

def test2(@consume c: Object^, f: (x: Object^) ->{c} Object^) =
  def cc: Object^ = c // error
  val x1 =
    { f(cc) } // error
  val x4: Object^ = // ^ hides just c, since the Object^ in the result of `f` is existential
    { f(c) } // error // error

def test3(@consume c: Object^, f: Object^ ->{c} Object^) =
  val x4: Object^ = // error: ^ hides f*, needs @consume
    { f(c) } // error

def test4(c: Object^, @consume f: Object^ ->{c} Object^) =
  val x4: Object^ = // error: ^ hides f* which refers to c, so c needs @consume
    { f(c) } // error












