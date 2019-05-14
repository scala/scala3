import Macro._

/**
  * These tests test some combinations that should make the f interpolator pass.
  * They come from https://github.com/lampepfl/dotty/pull/3894/files
  * The expected result is in the .check file (only need to modify "ff" by "f2" in that file)
  */

object Test2 {

  def testA = {
     def test1(n: Int) = {
       println(s"Bob is $n years old")
       println(f2"Bob is $n%2d years old")
       println(s"Bob will be ${n+1} years old")
       // println(f2"Bob will be ${n+1}%2d years old")
       println(s"$n+1 = ${n+1}")
       // println(f2"$n%d+1 = ${n+1}%d")
     }

    def test2(f: Float) = {
      println(s"Best price: $f")
      println(f2"Best price: $f%.2f")
      println(s"$f% discount included")
      println(f2"$f%3.2f%% discount included")
    }

    test1(1)
    test1(12)
    test1(123)

    test2(10.0f)
    test2(13.345f)

    println(s"")
    println(s"${0}")
    println(s"${0}${0}")
    println(f2"")
    println(f2"${0}")
    println(f2"${0}${0}")
  }

    // interpolationMultiline1.scala
   def testB = {
     def test1(n: Int) = {
       println(s"""Bob is $n years old""")
       println(f2"""Bob is $n%2d years old""")
       println(s"""Bob will be ${n+1} years old""")
       // println(f2"""Bob will be ${n+1}%2d years old""")
       println(s"""$n+1 = ${n+1}""")
       // println(f2"""$n%d+1 = ${n+1}%d""")
     }

      def test2(f: Float) = {
       println(s"""Best price: $f""")
       println(f2"""Best price: $f%.2f""")
       println(s"""$f% discount included""")
       println(f2"""$f%3.2f%% discount included""")
     }

      test1(1)
     test1(12)
     test1(123)

      test2(10.0f)
     test2(13.345f)
   }

    // interpolationMultiline2.scala
   def testC = {
     def test1(n: Int) = {
       val old = "old"
       val catcher: PartialFunction[Throwable, Unit] = { case e => println(e) }
       try { println(s"""Bob is ${s"$n"} years ${s"$old"}!""") } catch catcher
       // try { println(s"""Bob is ${f2"$n"} years ${s"$old"}!""") } catch catcher
       // try { println(f2"""Bob is ${s"$n"} years ${s"$old"}!""") } catch catcher
       // try { println(f2"""Bob is ${f2"$n"} years ${s"$old"}!""") } catch catcher
       // try { println(f2"""Bob is ${f2"$n%2d"} years ${s"$old"}!""") } catch catcher
       // try { println(f2"""Bob is ${s"$n%2d"} years ${s"$old"}!""") } catch catcher
       // try { println(s"""Bob is ${f2"$n%2d"} years ${s"$old"}!""") } catch catcher
       try { println(s"""Bob is ${s"$n%2d"} years ${s"$old"}!""") } catch catcher
     }

      test1(1)
     println("===============")
     test1(12)
     println("===============")
     test1(123)
   }

    def main(args: Array[String]): Unit = {
     println(f2"integer: ${5}%d")
     println(f2"string: ${"l"}%s")
     println(f2"${5}%s, ${6}%d, ${"hello"}%s")

      val x = 5
     println(f2"$x%d")
     // println(f2"${x + 1}%d")

      // ported from scalac
     testA
     testB
     testC
   }
 }