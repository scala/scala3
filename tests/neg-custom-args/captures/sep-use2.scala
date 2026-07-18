import caps.fresh

def test1(consume c: Object^, f: (x: Object^) => Object^{fresh}) =
  def cc: Object^ = c  // error
  val x1 =
    { f(cc) } // ok
  val x2 =
    f(cc)  // ok
  val x3: Object^ =
    f(cc) // ok
  val x4: Object^ =
    { f(c) } // error

def test2(consume c: Object^, f: (x: Object^) ->{c} Object^{fresh}) =
  def cc: Object^ = c // error
  val x1 =
    { f(cc) } // error
  val x4: Object^ = // ^ hides just c, since the Object^ in the result of `f` is existential
    { f(c) } // error // error

def test3(consume c: Object^, f: Object^ ->{c} Object^{fresh}) =
  val x4: Object^ = // was error: ^ hides f*, needs consume, now OK once we use fresh
    { f(c) } // error

def test4(c: Object^, consume f: Object^ ->{c} Object^{fresh}) =
  val x4: Object^ = // was error: ^ hides f* which refers to c, so c needs consume, now OK now OK once we use fresh
    { f(c) } // error












