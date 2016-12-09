package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Types.Type

case class TypeWithContext(tp: Type, outerTargs: OuterTargs)
