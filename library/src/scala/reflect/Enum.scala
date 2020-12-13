package scala.reflect

/** A base trait of all Scala enum definitions */
@annotation.transparentTrait trait Enum extends Any, Product, Serializable:

  /** A number uniquely identifying a case of an enum */
  def ordinal: Int
