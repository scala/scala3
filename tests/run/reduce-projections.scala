
class C(val x1: Int, val x2: Int, val x3: Int, val x4: Int)

object Test {

  class D(n: Int) {
    println(n)
    def result = n
  }
  object O2 extends D(2)
  object O2a extends D(2)
  object O2b extends D(2)
  object O3 extends D(3)
  object O3a extends D(3)
  object O3b extends D(3)

  transparent def f(): Unit = {
    println(new C(
      { println(1); 1 },
      { println(2); 2 },
      { println(3); 3 },
      { println(4); 4 }
    ).x1)
    println(new C(
      { println(1); 1 },
      { println(2); 2 },
      { println(3); 3 },
      { println(4); 4 }
    ).x2)
    println(new C(
      { println(1); 1 },
      { println(2); 2 },
      { println(3); 3 },
      { println(4); 4 }
    ).x3)
    println(new C(
      { println(1); 1 },
      { println(2); 2 },
      { println(3); 3 },
      { println(4); 4 }
    ).x4)
    println("===")
    println(new C(
      { 1 },
      { println(2); 2 },
      { println(3); 3 },
      { println(4); 4 }
    ).x1)
    println(new C(
      { println(1); 1 },
      { 2 },
      { println(3); 3 },
      { println(4); 4 }
    ).x2)
    println(new C(
      { println(1); 1 },
      { println(2); 2 },
      { 3 },
      { println(4); 4 }
    ).x3)
    println(new C(
      { println(1); 1 },
      { println(2); 2 },
      { println(3); 3 },
      { 4 }
    ).x4)
    println("===")
    println(new C(
      { 1 },
      { println(2); 2 },
      { println(3); 3 },
      { 4 }
    ).x1)
    println(new C(
      { 1 },
      { 2 },
      { println(3); 3 },
      { 4 }
    ).x2)
    println(new C(
      { 1 },
      { println(2); 2 },
      { 3 },
      { 4 }
    ).x3)
    println(new C(
      { 1 },
      { println(2); 2 },
      { println(3); 3 },
      { 4 }
    ).x4)
    println("===")
    println(new C(
      { 1 },
      O2.result,
      O3.result,
      { 4 }
    ).x1)
    println(new C(
      { 1 },
      { 2 },
      O3a.result,
      { 4 }
    ).x2)
    println(new C(
      { 1 },
      O2a.result,
      { 3 },
      { 4 }
    ).x3)
    println(new C(
      { 1 },
      O2b.result,
      O3b.result,
      { 4 }
    ).x4)
  }

  def main(args: Array[String]): Unit = {
    f()
  }
}