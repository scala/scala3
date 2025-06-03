object A:
  inline def inlinedParam(inline x: Int): Int = x + x

object Exported:
  export A.*
