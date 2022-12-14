class ToString(d:Double) extends AnyVal :
  inline override def toString():String = s"$d meters"

@main def Test =
  val ts = ToString(2.0)
  val a: Any = ts
  println(a)

