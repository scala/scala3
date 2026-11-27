class Res extends caps.SharedCapability:
  def use(): Unit = ()

// Transparent inline methods are expanded before capture checking.
// The two expansions must not share the inferred result type of `get`.
transparent inline def mk(r: Res): () => Unit =
  def get = () => r.use()
  get

def test1(a: Res): () ->{a} Unit = mk(a)
def test2(b: Res): () ->{b} Unit = mk(b)
