package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type

case class MethodSummary(methodDef: Symbol,
                         thisAccessed: Boolean,
                         methodsCalled: Map[Type, List[CallInfo]],
                         // allocatedLambdas
                         accessedModules: List[Symbol],
                         argumentReturned: Byte, // -1 if not known
                         argumentStoredToHeap: List[Boolean] // not currently collected
                        )
