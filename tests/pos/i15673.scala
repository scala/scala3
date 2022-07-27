trait SeqOps[+A, +CC[_], +C]:   // scala.collection.SeqOps
  def reverse: C

extension[A, CC[B] <: SeqOps[B, CC, CC[B]]](ring: CC[A])
  def startAt(i: Int): CC[A] = ???
  def reflectAt(i: Int): CC[A] =
    startAt(i).reverse