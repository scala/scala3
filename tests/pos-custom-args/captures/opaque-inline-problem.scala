trait Async extends caps.SharedCapability:
  def group: Int

object Async:
  inline def current(using async: Async): async.type = async
  opaque type Spawn <: Async = Async
  def blocking[T](f: Spawn ?=> T): T = ???

def main() =
  Async.blocking:
    val a = Async.current.group