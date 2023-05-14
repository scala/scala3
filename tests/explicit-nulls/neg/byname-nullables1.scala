def f(op: => Boolean): Unit = ()
def f(op: Int): Unit = ()

class C:
  var fld: String | Null = null

def test() =
  var x: C | Null = C()
  if x != null then
    f(x.fld != null) // error
    require(x.fld != null, "error")    // ok