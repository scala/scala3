
import caps.consume
import caps.unsafe.unsafeAssumeSeparate

def Test(@consume c: Object^, f: Object^ => Object^) =
  def cc: Object^ = unsafeAssumeSeparate(c)
  val x1 =
    { f(cc) }
  val x2 =
    f(cc)
  val x3: Object^ =
    f(cc)
  val x4: Object^ =
    { f(cc) }





