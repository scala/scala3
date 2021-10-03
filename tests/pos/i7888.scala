def usingSeq[B](f: [A] => Seq[A] => B): B = {
  f(Nil)
}
def crash() = {
  usingSeq { [A] => (a: Seq[A]) =>
    a
  }
}
