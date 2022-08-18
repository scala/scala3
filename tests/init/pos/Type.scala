class Type:
  def underlyingType =
    val isProxy = this.isInstanceOf[TypeProxy]
    if (isProxy) this.asInstanceOf[TypeProxy].underlying
    else NoType

  def underlyingName =
    val isProxy = this.isInstanceOf[TypeProxy]
    if (isProxy) this.asInstanceOf[TypeProxy].name
    else "<empty>"

abstract class TypeProxy extends Type:
  def underlying: Type = this
  val name: String

object NoType extends Type

object Implicits:
  object NoImplicits extends Type
  println(NoImplicits)
  val n = 10
