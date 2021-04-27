object Macro {
  inline def expand(): Unit = ${impl}
  def impl(using scala.quoted.Quotes) = '{???}
}