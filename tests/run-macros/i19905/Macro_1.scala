import scala.quoted.*

inline def noPrefixShortCode: String =
  ${ noPrefixShortCodeImpl }

inline def noPrefixCode: String =
  ${ noPrefixCodeImpl }

inline def noPrefixStructure: String =
  ${ noPrefixStructure }

def noPrefix(using Quotes): quotes.reflect.TypeRepr =
  import quotes.reflect.*
  TypeRepr.of[Unit] match
    case TypeRef(ThisType(TypeRef(prefix, _)), _) => prefix

def noPrefixShortCodeImpl(using Quotes): Expr[String] =
  import quotes.reflect.*
  try Expr(Printer.TypeReprShortCode.show(noPrefix))
  catch case ex: Exception =>
    Expr(s"${ex.getClass.getName}: ${ex.getMessage}")

def noPrefixCodeImpl(using Quotes): Expr[String] =
  import quotes.reflect.*
  try Expr(Printer.TypeReprCode.show(noPrefix))
  catch case ex: Exception =>
    Expr(s"${ex.getClass.getName}: ${ex.getMessage}")

def noPrefixStructure(using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr(Printer.TypeReprStructure.show(noPrefix))
