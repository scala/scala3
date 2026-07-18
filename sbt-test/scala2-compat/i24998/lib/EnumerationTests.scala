object EnumerationTests {
  object E extends Enumeration {
    val A, B = Value
  }

  def apply(): Unit = {
    val builder = E.ValueSet.newBuilder
    builder += E.A // error
    builder += E.B // error
    val vs = builder.result()
    assert(vs.contains(E.A))
    assert(vs.contains(E.B))
  }
}
