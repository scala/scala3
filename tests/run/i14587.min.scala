object Test:
  def test(cond: Boolean) =
    val tup: (String, Unit) | (Int, Unit) = if cond then ("", ()) else (1, ())
    tup match
      case (s: String, _) => s
      case _ => "n/a"

  def main(args: Array[String]): Unit =
    test(true)  // works
    test(false) // was: ClassCastException: class scala.None$ cannot be cast to class scala.Some
