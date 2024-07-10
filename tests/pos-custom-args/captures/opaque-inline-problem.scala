trait Async extends caps.Capability:
  def group: Int

object Async:
  inline def current(using async: Async): async.type = async
  opaque type Spawn <: Async = Async
  def blocking[T](f: Spawn => T): T = ???

def main() =
  Async.blocking: spawn =>
    val c = Async.current(using spawn)
    val a = c.group
