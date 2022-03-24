package covtest

class A(val x: Int, val y: Int)
class B(x: Int) extends A(x, 0)
class C1 extends B({println("block"); 1})
class C2 extends B(A(2,2).x)

def testInheritance =
  println(C1().x) // block
                  // 1
  println(C2().x) // 2
