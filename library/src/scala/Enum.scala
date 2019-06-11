package scala

/** A base trait of all enum classes */
trait Enum {

  /** A number uniquely identifying a case of an enum */
  def ordinal: Int
  protected def $ordinal: Int
}
