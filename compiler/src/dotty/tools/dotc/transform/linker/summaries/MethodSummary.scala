package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.transform.linker.types.ClosureType

class MethodSummary(val methodDef: Symbol,
                    val thisAccessed: Boolean,
                    val methodsCalled: Map[Type, List[CallInfo]],
                    val definedClosures: List[ClosureType],
                    val accessedModules: List[Symbol],
                    val argumentReturned: Byte, // -1 if not known
                    val argumentStoredToHeap: List[Boolean] // not currently collected
                   )
