object A {
  def unapplySeq(a: Any): Seq[_] = ""
}

def unapply(x: Any) = x match { case A() => }
