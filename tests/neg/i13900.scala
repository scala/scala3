opaque type Inlined[T] = T

object Inlined:

  given fromValueWide[Wide]: Conversion[Wide, Inlined[Wide]] = ???

  // TODO: This used to make the compiler run into an infinite loop.
  // Now it fails instead but shouldn't, see discussion in https://github.com/lampepfl/dotty/issues/13900#issuecomment-1075580792
  def myMax: Int = 1 max 2 // error
