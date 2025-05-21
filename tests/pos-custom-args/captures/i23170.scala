import caps.*

trait A

extension (a: A^{cap.rd})
  def await = ()

def awaitA[C <: {cap.rd}](a: A^{C}) = a.await