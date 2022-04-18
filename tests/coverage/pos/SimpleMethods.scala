package covtest

class C:
  def a: C = this
  def b: Unit = return
  def c: Unit = ()
  def d: Int = 12
  def e: Null = null

  def block: Int =
    "literal"
    0

  def cond: Boolean =
    if false then true
    else false

  def new1: C = new {}

  def tryCatch: Unit =
    try ()
    catch
      case e: Exception => 1
