// TODO: this should be a compilation error
// program fails at runtime with java.lang.InstantiationError
// see tests/pending/neg/i5690.scala
object AnnotInst{
  def main(a: Array[String]) = {
    new java.lang.annotation.Inherited
  }
}
