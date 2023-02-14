
// was pos/t2799.scala
// scalac: -deprecation -Werror

@deprecated("hi mom", "") case class Bob ()

@deprecated("other mother", "")
trait T

object T extends T {
  def t = Bob()
}
