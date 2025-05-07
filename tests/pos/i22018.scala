class SelectableNT[A <: NamedTuple.AnyNamedTuple](val nt: A) extends Selectable:
  type Fields = A
  def selectDynamic(x: String) = ???

object Test:

  val a = (name = "foo", age = 1)

  val sa = SelectableNT(a)
  sa.name   // ok

  type B = a.type
  val b: B = a

  val sb = SelectableNT(b)
  sb.name   // fails