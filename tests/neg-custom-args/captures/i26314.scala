trait Async extends caps.SharedCapability:
  def use(): Unit = ()

object Async:
  final opaque type Spawn <: Async = Async

def pure(spawn: Async.Spawn): Async^{} = spawn // error

def outerLost(a: Async, b: Async)[T <: Async^{b}](x: T^{a}): Async^{b} = x // error

def innerLostInClosure(a: Async, b: Async)[T <: Async^{b}](x: T^{a}): () ->{a} Unit =
  () => x.use() // error
