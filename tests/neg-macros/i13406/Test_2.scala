// Test_2.scala
trait Bar

inline given derivedReducible(using scala.deriving.Mirror.SumOf[Qux[?]]): Bar =
  scala.compiletime.summonInline[Bar]
  ???

def test = derivedReducible // error
