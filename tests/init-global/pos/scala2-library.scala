//> using options -Ycompile-scala2-library
case class UninitializedFieldError(msg: String) extends RuntimeException(msg)
