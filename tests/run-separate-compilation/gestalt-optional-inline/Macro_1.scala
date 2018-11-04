// Port of https://github.com/liufengyun/gestalt/blob/master/macros/src/main/scala/gestalt/macros/Optional.scala
// using only `inline`

final class Optional[+A >: Null](val value: A) extends AnyVal {
  def get: A = value
  def isEmpty = value == null

  inline def getOrElse[B >: A](alt: => B): B =
     if (isEmpty) alt else value

  // f is by name to beta-reduce it
  inline def map[B >: Null](f: => A => B): Optional[B] = {
    if (isEmpty) new Optional(null)
    else new Optional(f(value))
  }

  override def toString = if (isEmpty) "<empty>" else s"$value"
}
