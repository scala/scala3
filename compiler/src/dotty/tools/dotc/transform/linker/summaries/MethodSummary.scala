package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type

import scala.collection.mutable

case class MethodSummary(methodDef: Symbol,
                         var thisAccessed: Boolean,
                         methodsCalled: mutable.Map[Type, List[CallInfo]],
                         // allocatedLambdas
                         var accessedModules: List[Symbol],
                         argumentReturned: Byte, // -1 if not known
                         var argumentStoredToHeap: List[Boolean] // not currently collected
                        )
