package scala

/** A base trait of all enum classes */
trait Enum {

  /** A number uniquely identifying a case of an enum */
  def ordinal: Int

  /** Name of the enum's case */
  def name: String
}
