trait T { def f: Int }
def impossible(x: Any): Unit =
  val y = x

def test: Unit =
  val x, x2, x3, x4 = ""

  if x != null then
    if x == null then impossible(new T{})

    if x == null then ()
    else
      if x == null then impossible(new T{})

  if x == null || {
    if x == null then impossible(new T{})
    true
  }
  then ()

  if x != null && {
    if x == null then impossible(new T{})
    true
  }
  then ()

  if !(x == null) && {
    if x == null then impossible(new T{})
    true
  }
  then ()

  x match
    case _: String =>
      if x == null then impossible(new T{})

  val y: Any = List(x)
  y match
    case y1 :: ys => if y == null then impossible(new T{})
    case Some(_) | Seq(_: _*) => if y == null then impossible(new T{})

  x match
    case null =>
    case _ => if x == null then impossible(new T{})

  if x == null then return
  if x == null then impossible(new T{})

  if x2 == null then throw AssertionError()
  if x2 == null then impossible(new T{})

  if !(x3 != null) then throw AssertionError()
  if x3 == null then impossible(new T{})

  assert(x4 != null)
  if x4 == null then impossible(new T{})
