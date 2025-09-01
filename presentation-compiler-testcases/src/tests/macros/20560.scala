package tests.macros

import scala.quoted.{Expr, Quotes}

object Macro20560:
  transparent inline def loadJavaSqlDriver: Int = ${ loadJavaSqlDriverImpl }

  private def loadJavaSqlDriverImpl(using Quotes): Expr[42] =
    Class.forName("java.sql.Driver")
    '{42}

  transparent inline def loadJavaSqlInexisting: Int = ${ loadJavaSqlInexistingImpl }

  private def loadJavaSqlInexistingImpl(using Quotes): Expr[42] =
    Class.forName("java.sql.Inexisting")
    '{42}
