package scala

/** A base trait of all enum classes */
trait Enum extends Product, Serializable:

  /** A string uniquely identifying a case of an enum */
  final def enumLabel: String = productPrefix

  /** A number uniquely identifying a case of an enum */
  def ordinal: Int
