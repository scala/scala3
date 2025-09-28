import language.experimental.captureChecking

trait MyMap[K, V]:
  def filterNot(pred: ((K, V)) => Boolean): MyMap[K, V]^{this, pred} = ???
