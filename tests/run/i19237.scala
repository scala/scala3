package mono {
  trait C
  object Focus:
    import internals.FocusImpl
    class MkFocus[From]: // This class only has methods of object at runtime
      transparent inline def apply[To](inline f: C ?=> From => To): Any =
        ${ FocusImpl('f) } // no accessor should be generated

  package internals {
    import scala.quoted.*

    private[mono] object FocusImpl:
      def apply[From: Type, To: Type](f: Expr[C ?=> From => To])(using Quotes): Expr[Any] =
        ???
  }
}

@main def Test =
  new mono.Focus.MkFocus[Any].getClass().getMethods().toList
    .sortBy(_.getName)
    .foreach(println)
