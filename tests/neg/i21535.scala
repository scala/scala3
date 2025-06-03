def test() = {
  val p = 10.toShort
  (if (true) then
    new A(66)
  else
    m1()
  ).m2(p1 = p); // error

}

def m1(): A[Short] = new A(10)

class A[D](var f: D) {

  def m2(p1: D = f, p2: D = f): Unit = {}
}