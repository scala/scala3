package scala.reflect

/** A base trait of all Scala enum definitions */
@annotation.transparentTrait trait Enum extends Any, Product, Serializable:

  /** A number uniquely identifying a case of an enum */
  def ordinal: Int

/** A base trait of all Scala enum companion definitions */
@annotation.transparentTrait trait EnumCompanion[E <: Enum] extends AnyRef

/** A base trait of all Scala singleton enum companion definitions */
@annotation.transparentTrait trait SingletonEnumCompanion[E <: Enum] extends EnumCompanion[E]:
  def values : Array[E]
  def valueOf(name : String) : E