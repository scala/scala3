import scala.quoted.*

inline def printTypeShort[T]: String =
  ${ printTypeShortImpl[T] }

inline def printType[T]: String =
  ${ printTypeImpl[T] }

inline def printTypeAnsi[T]: String =
  ${ printTypeAnsiImpl[T] }

inline def printTypeStructure[T]: String =
  ${ printTypeStructureImpl[T] }

def printTypeShortImpl[T: Type](using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr(Printer.TypeReprShortCode.show(TypeRepr.of[T]))

def printTypeImpl[T: Type](using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr(Printer.TypeReprCode.show(TypeRepr.of[T]))

def printTypeAnsiImpl[T: Type](using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr(Printer.TypeReprAnsiCode.show(TypeRepr.of[T]))

def printTypeStructureImpl[T: Type](using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr(Printer.TypeReprStructure.show(TypeRepr.of[T]))
