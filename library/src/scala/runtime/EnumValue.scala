package scala.runtime

@annotation.transparentTrait trait EnumValue extends Product, Serializable:
  override def canEqual(that: Any) = this eq that.asInstanceOf[AnyRef]
  override def productArity: Int = 0
  override def productElement(n: Int): Any =
    throw IndexOutOfBoundsException(n.toString)
  override def productElementName(n: Int): String =
    throw IndexOutOfBoundsException(n.toString)
