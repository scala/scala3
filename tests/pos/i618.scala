class C(val f: Any*)

class D(override val f: Nothing) extends C(f)
