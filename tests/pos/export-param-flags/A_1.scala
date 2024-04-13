object A:
  def defaultParam(x: Int = 1) = x

object Exported:
  export A.*
