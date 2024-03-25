object Example {
  def test[A, B](a: A, b: B): A | B = a

  val v0 = test("string", 1)
  val v1 = test(1, "string")
  val v2 = test(v0, v1)
  val v3 = test(v1, v0)
  val v4 = test(v2, v3)
  val v5 = test(v3, v2)
  val v6 = test(v4, v5)

  def d0 = test("string", 1)
  def d1 = test(1, "string")
  def d2 = test(d0, d1)
  def d3 = test(d1, d0)
  def d4 = test(d2, d3)
  def d5 = test(d3, d2)
  def d6 = test(d4, d5)
}