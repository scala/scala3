import language.experimental.captureChecking

type AnyIterableOnce[A] = IterableOnce[A]^

/** Iterator can be used only once */
trait IterableOnce[+A]:
  //this: IterableOnce[A]{ref any} =>
  this: AnyIterableOnce[A] =>
  def iterator: Iterator[A]^{this}
