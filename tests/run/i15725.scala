class ToString(d: Int) extends AnyVal :
  inline override def toString():String = s"$d meters"

@main def Test =
  val ts = ToString(2)
  val a: Any = ts
  println(a)

