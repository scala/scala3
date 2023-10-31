class A {
  class C[x]
  val cs = new scala.collection.mutable.HashMap[C[?], Int]
  def c: C[?] = sys.error("")
  val eval: C[?] = c
  cs(c) += 1
}
