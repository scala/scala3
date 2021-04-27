import scala.annotation.targetName

trait Example:
  @targetName("funfun")
  inline def fun: Unit = ???
