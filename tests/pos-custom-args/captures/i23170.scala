import caps.*

trait A

extension (a: A^{any.rd})
  def await = ()

def awaitA[C^ <: {any.rd}](a: A^{C}) = a.await