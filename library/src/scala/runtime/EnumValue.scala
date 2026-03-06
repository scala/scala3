package scala.runtime

import language.experimental.captureChecking

transparent trait EnumValue extends Any, Product, Serializable:
  infix def veq(that: Any): Boolean = {
    if(this.isInstanceOf[AnyRef] && that.isInstanceOf[AnyRef]){
      this.asInstanceOf[AnyRef] eq that.asInstanceOf[AnyRef]
    }
    else if(this.isInstanceOf[AnyVal] && that.isInstanceOf[AnyVal]){
      this == that
    }
    else{
      false
    }
  }

  override def canEqual(that: Any) = this veq that
  override def productArity: Int = 0
  override def productElement(n: Int): Any =
    throw IndexOutOfBoundsException(n.toString)
  override def productElementName(n: Int): String =
    throw IndexOutOfBoundsException(n.toString)
