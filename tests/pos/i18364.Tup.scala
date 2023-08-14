// Capturing the regression will implementing the fix for i18364
// That broke in CI, "case _" "Unreachable case except for null"
// Because IArray is an opaque alias of Array
object Tup:
  /** Convert an immutable array into a tuple of unknown arity and types */
  def fromIArray[T](xs: IArray[T]): Tuple =
    val xs2: IArray[Object] = xs match
      case xs: IArray[Object] @unchecked => xs
      case _ => xs.map(_.asInstanceOf[Object])
    runtime.Tuples.fromIArray(xs2)
