import language.experimental.captureChecking

trait MyMap[K, V]:
  def filterNot(pred: ((K, V)) => Boolean): MyMap[K, V]^{this, pred} = ???

trait MySeq[+T]:
  def map[U](f: T => U): MySeq[U]^{this, f} = ???
