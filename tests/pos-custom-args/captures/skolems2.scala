import language.future // sepchecks on
import caps.consume

def Test(@consume c: Object^, f: Object^ => Object^) =
  def cc: Object^ = c
  val x1 =
    { f(cc) }
  val x2 =
    f(cc)
  val x3: Object^ =
    f(cc)
  val x4: Object^ =
    { f(cc) }





