// Macro class
package test

import scala.quoted.*

def notMacro(bool: Expr[Bool])(using Quotes): Expr[Bool] =
  '{$bool(False, True)}

def showMacro(bool: Expr[Bool])(using Quotes): Expr[String] =
  '{$bool("TRUE", "FALSE")}

def foldMacro[T: Type](bool: Expr[Bool], t: Expr[T], f: Expr[T])(using Quotes): Expr[T] =
  '{$bool($t, $f)}