object Test:
  def main(args: Array[String]): Unit =
    test(1) // was: ClassCastException: java.lang.Integer cannot be cast to scala.runtime.TupleXXL

  def test(any: Any) = any match
    case (
      _, _, _, _, _, _, _, _, _, _,
      _, _, _, _, _, _, _, _, _, _,
      _, _, _
    ) => 23
    case _ => -1
