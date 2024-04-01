//> using options -Ysafe-init-global -Ycompile-scala2-library
case class UninitializedFieldError(msg: String) extends RuntimeException(msg)
