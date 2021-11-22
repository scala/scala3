package dotty.tools.dotc.sbt

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.NameOps.stripModuleClassSuffix
import dotty.tools.dotc.core.Names.Name

inline val TermNameHash = 1987 // 300th prime
inline val TypeNameHash = 1993 // 301st prime
inline val EmptyParamHash = 1997 // 302nd prime
inline val InlineParamHash = 1999 // 303rd prime
inline val InlineValHash = 2003 // 304th prime

extension (sym: Symbol)

  def constructorName(using Context) =
    sym.owner.fullName ++ ";init;"

  /** Mangle a JVM symbol name in a format better suited for internal uses by sbt. */
  def zincMangledName(using Context): Name =
    if (sym.isConstructor) constructorName
    else sym.name.stripModuleClassSuffix
