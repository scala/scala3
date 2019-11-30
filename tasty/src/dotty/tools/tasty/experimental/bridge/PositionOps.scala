package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait PositionOps extends Core with

  given SourcePosition: (pos: SourcePosition) with
    def line: Int = internal.SourcePosition_line(pos)
