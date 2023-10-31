object A {
  def unapplySeq(a: Any): Seq[?] = ""
}

def unapply(x: Any) = x match { case A() => }
