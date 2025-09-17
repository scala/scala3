import language.experimental.captureChecking
trait Op:
  def run(): Unit
def helper[X <: Op^](op: X): () -> Unit =
  () => op.run()  // error
def test1(a: Op^): Unit =
  def foo[X <: Op^{a}](op: X): () ->{a} Unit =
    () => op.run()  // ok
  def bar[X <: Op^{a}](op: X): () ->{} Unit =
    () => op.run()  // error
