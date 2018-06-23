
class C(val x1: Int, val x2: Int, val x3: Int, val x4: Int)

object Test {
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
  }

  def main(args: Array[String]): Unit = {
    f()
  }
}