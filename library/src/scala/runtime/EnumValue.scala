package scala.runtime

import language.experimental.captureChecking

transparent trait EnumValue extends Product, Serializable:
  /** Returns `true` if `that` is the same object as this one, by reference.
   *
   *  Each simple enum case is a singleton, so it can only compare equal to
   *  itself.
   *
   *  @param that the value to compare with this enum value
   */
  override def canEqual(that: Any) = this eq that.asInstanceOf[AnyRef]
  /** Returns `0`: a simple enum case has no case fields. */
  override def productArity: Int = 0
  /** Always throws: a simple enum case has no elements.
   *
   *  @param n the index of the requested element; no index is valid
   *  @throws IndexOutOfBoundsException always, with `n` as its message
   */
  override def productElement(n: Int): Any =
    throw IndexOutOfBoundsException(n.toString)
  /** Always throws: a simple enum case has no elements.
   *
   *  @param n the index of the requested element name; no index is valid
   *  @throws IndexOutOfBoundsException always, with `n` as its message
   */
  override def productElementName(n: Int): String =
    throw IndexOutOfBoundsException(n.toString)
