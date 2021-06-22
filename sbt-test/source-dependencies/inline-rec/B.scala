class B {
  def main(args: Array[String]): Unit = {
    assert(A.callInline == C.expected)
  }
}
