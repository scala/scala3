import scala.quoted.*

inline def fail(): Unit = ${ impl }

private def impl(using Quotes) : Expr[Unit] =
  // should never be done without reporting error before (see docs)
  throw new scala.quoted.runtime.StopMacroExpansion
