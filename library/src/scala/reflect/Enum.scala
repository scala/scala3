package scala.reflect

/** A base trait of all Scala enum definitions */
@annotation.transparentTrait trait Enum extends Any, Product, Serializable:

  /** A number uniquely identifying a case of an enum */
  def ordinal: Int

//@annotation.transparentTrait trait EnumCompanion extends AnyRef

/** A base trait of all Scala enum companion definitions */
@annotation.transparentTrait trait EnumCompanion[E <: Enum] extends AnyRef:

  def values : Array[E]
  def valueOf(name : String) : E