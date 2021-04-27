object Test extends App {
  assert(x == 10)
  assert(x("abc") == 3)
  assert((new x("abc"){}).s == "abc")

  assert(y("abc") == 3)
  assert(yy.foo == 3)

  val x2: X2 = "abc"
  assert(X2.bar == "hi")

  val x3: X3 = new X3
}