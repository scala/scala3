// instantiating java annotations should not be allowed 
// (would fail at runtime with java.lang.InstantiationError)
// see tests/pos/i5690.scala
object AnnotInst{
  def main(a: Array[String]) = {
    new java.lang.annotation.Inherited // error
  }
}
