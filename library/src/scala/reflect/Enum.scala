package scala.reflect

import language.experimental.captureChecking

/** A base trait of all Scala enum definitions. */
transparent trait Enum extends Any, Product, Serializable:

  /** A number uniquely identifying a case of an enum. */
  def ordinal: Int
