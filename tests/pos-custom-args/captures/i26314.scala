trait Async extends caps.SharedCapability:
  def use(): Unit = ()

object Async:
  final opaque type Spawn <: Async = Async
  def blocking[T](op: Spawn ?=> T): T = op(using new Async {})

def f(using Async): Int = 1

def g: Int =
  Async.blocking:
    f

def callParam(b: Async)[T <: Async^{b}](t: T): Int = f(using t)

trait Abstract:
  type Spawn <: Async
  def call(t: Spawn): Int = f(using t)

def closure(a: Async, b: Async)[T <: Async^{b}](x: T^{a}): () ->{a, b} Unit =
  () => x.use()
