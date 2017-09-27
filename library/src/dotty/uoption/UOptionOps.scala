package dotty.uoption

class UOptionOps[A](private val self: UOption[A]) extends AnyVal {
  @inline def isEmpty: Boolean = self eq UNone
  @inline def nonEmpty: Boolean = !isEmpty
  @inline def isDefined: Boolean = !isEmpty

  /** Must not be called when `isEmpty` is `true`! */
  // @inline // only for Scala.js?
  private def forceGet: A = (self: Any) match {
    case none: WrappedNone =>
      none.unwrap.asInstanceOf[A]
    case _ =>
      self.asInstanceOf[A]
  }

  @inline // is this a good idea at all?
  def get: A =
    if (isEmpty) throw new NoSuchElementException("UNone.get")
    else forceGet

  @inline def map[B](f: A => B): UOption[B] =
    if (isEmpty) self.asInstanceOf[UOption[B]]
    else USome(f(forceGet))

  @inline def flatMap[B](f: A => UOption[B]): UOption[B] =
    if (isEmpty) UNone else f(forceGet)

  def flatten[B](implicit ev: A <:< UOption[B]): UOption[B] =
    if (isEmpty) UNone else ev(forceGet)

  @inline final def fold[B](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(forceGet)

  @inline def filter(p: A => Boolean): UOption[A] =
    if (isEmpty || p(forceGet)) self else UNone

  @inline def withFilter(p: A => Boolean): WithFilter[A] = new WithFilter[A](self, p)

  @inline def find(p: A => Boolean): UOption[A] = filter(p)

  @inline def exists(p: A => Boolean): Boolean =
    !isEmpty && p(forceGet)

  @inline def forall(p: A => Boolean): Boolean =
    isEmpty || p(forceGet)

  @inline def getOrElse[B >: A](ifEmpty: => B): B =
    if (isEmpty) ifEmpty else forceGet

  @inline final def orElse[B >: A](alternative: => UOption[B]): UOption[B] =
    if (isEmpty) alternative else self.asInstanceOf[UOption[B]]

  @inline final def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = getOrElse(ev(null))

  @inline def foreach[U](f: A => U): Unit = {
    if (isDefined) f(forceGet)
  }

  @inline def toSeq: Seq[A] = iterator.toSeq
  @inline def toList: List[A] = iterator.toList

  @inline def iterator: Iterator[A] =
    if (isEmpty) Iterator.empty
    else Iterator.single(forceGet)

  @deprecated("", "")
  def toOption: Option[A] =
    if (isEmpty) None
    else Some(forceGet)
}
