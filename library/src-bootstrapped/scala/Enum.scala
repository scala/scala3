package scala

/** A base trait of all enum classes */
trait Enum extends Product, Serializable:

  /** A number uniquely identifying a case of an enum */
  def ordinal: Int
