trait T:
  val x: Option[Int] = None
  val seenInInit: Option[Int] = x

class C extends T:
  override val x: Some[Int] = Some(1)

@main def Test =
  try println(C().seenInInit)
  catch case e: ClassCastException => println(e.getClass.getName)
