import scala.quoted._

inline def ofMapKeyValues: Map[String, String] = ${ ofMapKeyValuesImpl }
private def ofMapKeyValuesImpl(using QuoteContext): Expr[Map[String, String]] =
  Expr.ofMapKeyValues(Map(
    '{ "foo" } -> '{ "bar" },
    '{ "a" } -> '{ "b" }
  ))

inline def ofMapKeys: Map[String, String] = ${ ofMapKeysImpl }
private def ofMapKeysImpl(using QuoteContext): Expr[Map[String, String]] =
  Expr.ofMapKeys(Map(
    '{ "foo" } -> "bar",
    '{ "a" } -> "b"
  ))

inline def ofMapValues: Map[String, String] = ${ ofMapValuesImpl }
private def ofMapValuesImpl(using QuoteContext): Expr[Map[String, String]] =
  Expr.ofMapValues(Map(
    "foo" -> '{ "bar" },
    "a" -> '{ "b" }
  ))
