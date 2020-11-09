package scala.reflect
import annotation.mixin

/** A base trait of all Scala enum definitions */
@mixin trait Enum extends Any, Product, Serializable:

  /** A number uniquely identifying a case of an enum */
  def ordinal: Int
