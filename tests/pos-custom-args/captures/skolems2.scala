

import caps.unsafe.unsafeAssumeSeparate

def Test(consume c: Object^, consume f: Object^ => Object^) =
  def cc: Object^ = unsafeAssumeSeparate(c)
  val x1 =
    { f(cc) }
  val x2 =
    f(cc)
  val x3: Object^ =
    f(cc)
  val x4: Object^ =
    { unsafeAssumeSeparate(f)(cc) }

def Test2(consume c: Object^, consume f: Object^ => Object^) =
  def cc(): Object^ = unsafeAssumeSeparate(c)
  val x1 =
    { f(cc()) }
  val x2 =
    f(cc())
  val x3: Object^ =
    f(cc())
  val x4: Object^ =
    { unsafeAssumeSeparate(f)(cc()) }





