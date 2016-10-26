package dotty.tools.dotc.transform.linker

import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.transform.linker.Summaries.{CallWithContext, Cast, TypeWithContext}

case class CallGraph(reachableMethods: Set[CallWithContext],
                     reachableTypes: Set[TypeWithContext],
                     casts: Set[Cast],
                     classOfs: Set[Symbol],
                     outerMethods: Set[Symbol])
