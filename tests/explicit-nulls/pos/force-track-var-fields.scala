import scala.annotation.nullTrackable

class A:
  @nullTrackable var s: String | Null = null
  def getS: String =
    if s == null then s = ""
    s

def test(a: A): String =
  if a.s == null then
    a.s = ""
    a.s
  else
    a.s