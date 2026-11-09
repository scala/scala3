package dotty.tools.directives

/** Position within a source file. Lines and columns are 0-based. */
case class Position(line: Int, column: Int, offset: Int)
