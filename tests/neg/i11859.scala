class I
case class C(x: I ?=> Object)  // error: case class element cannot be a context function

type ITO = I ?=> Object
case class D(x: ITO)  // error: case class element cannot be a context function

@main def Test =
  val c = C(new Object())
  assert(c.hashCode == c.hashCode())
