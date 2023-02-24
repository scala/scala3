import scala.quoted.*
import scala.annotation.binaryAPI

inline def fail(): Unit = ${ impl }

@binaryAPI private def impl(using Quotes) : Expr[Unit] =
  // should never be done without reporting error before (see docs)
  throw new scala.quoted.runtime.StopMacroExpansion
