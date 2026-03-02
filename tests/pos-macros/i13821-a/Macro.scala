trait Conversions:
  given conv(using DFBits.Candidate): Conversion[Int, DFVal] = ???

import scala.quoted.*
transparent inline def f: Short = ${ getWidthMacro }
private def getWidthMacro(using Quotes): Expr[Short] = '{ ??? }
