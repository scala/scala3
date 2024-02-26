package x

import scala.language.experimental.mode
import scala.quoted.*

object M:

  inline def settingsContains(inline x:String): Boolean = ${
     settingsContainsImpl('x)
  }

  def settingsContainsImpl(x:Expr[String])(using Quotes): Expr[Boolean] =
     import quotes.reflect.*
     val v = x.valueOrAbort
     val r = CompilationInfo.XmacroSettings.contains(v)
     Expr(r)
