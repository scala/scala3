package scala

trait EnumValue extends Product, Serializable:
  override def canEqual(that: Any) = true
  override def productArity: Int = 0
  override def productPrefix: String = toString
  override def productElement(n: Int): Any =
    throw IndexOutOfBoundsException(n.toString())
  override def productElementName(n: Int): String =
    throw IndexOutOfBoundsException(n.toString())
