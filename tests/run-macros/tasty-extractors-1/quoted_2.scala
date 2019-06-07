
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    printTree(true)
    printTree(1)
    printTree(2L)
    printTree(2.1f)
    printTree(2.2d)
    printTree("abc")
    printTree(println("abc"))
    printTree(8: Int)
    printTree(8: Byte)
    printTree(8: Short)
    printTree('a')
    printTree { 1; 2; 3 }
    printTree(if (true: Boolean) 1 else 2)
    printTree("a" match { case "a" => () })
    printTree("b" match { case n => () })
    printTree("c" match { case n: String => () })
    printTree("e" match { case _ => () })
    printTree("f" match { case _: String => () })
    printTree(("g": Any) match { case _: String | _: Int => () })
    printTree("h" match { case _ if false => () })
    printTree { val a = "o"; "i" match { case a => () } }
    // printTree(Option(4) match { case Some(a) => a; case None => 1 })
    printTree(Nil match { case List(a, b, c) => })
    printTree(try 1 catch { case _ => })
    printTree(try 2 finally ())
    printTree(try 3 catch { case _ => } finally ())
    printTree("a" == "b")
    printTree(new Object)
    printTree(Int.box(x = 9))
    printTree(Ordering.apply[Int])
    printTree { val a: Int = 3 }
    printTree { lazy val b: Int = 3 }
    printTree { def f1: Int = 3 }
    printTree { def f2: Int = return 4 }
    printTree { def f3(i: Int): Int = i }
    printTree { def f4(i: Int)(j: Int): Int = i + j }
    printTree { def f5(i: Int = 9): Int = i }
    printTree { def f6[T](x: T): T = x }
    printTree { def f7[T](x: T): x.type = x }
    printTree { def f8(i: Int*): Int = 9; f8(1, 2, 3) }
    printTree { def f9(i: => Int): Int = i }
  }
}
