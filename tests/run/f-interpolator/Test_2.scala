
import FInterpolation._

object Test {
  // interpolation.scala
  def testA = {
    def test1(n: Int) = {
      println(s"Bob is $n years old")
      println(ff"Bob is $n%2d years old")
      println(s"Bob will be ${n+1} years old")
      // println(ff"Bob will be ${n+1}%2d years old")
      println(s"$n+1 = ${n+1}")
      // println(ff"$n%d+1 = ${n+1}%d")
    }

    def test2(f: Float) = {
      println(s"Best price: $f")
      println(ff"Best price: $f%.2f")
      println(s"$f% discount included")
      println(ff"$f%3.2f%% discount included")
    }

    test1(1)
    test1(12)
    test1(123)

    test2(10.0f)
    test2(13.345f)

    println(s"")
    println(s"${0}")
    println(s"${0}${0}")
    println(ff"")
    println(ff"${0}")
    println(ff"${0}${0}")
  }

  // interpolationMultiline1.scala
  def testB = {
    def test1(n: Int) = {
      println(s"""Bob is $n years old""")
      println(ff"""Bob is $n%2d years old""")
      println(s"""Bob will be ${n+1} years old""")
      // println(ff"""Bob will be ${n+1}%2d years old""")
      println(s"""$n+1 = ${n+1}""")
      // println(ff"""$n%d+1 = ${n+1}%d""")
    }

    def test2(f: Float) = {
      println(s"""Best price: $f""")
      println(ff"""Best price: $f%.2f""")
      println(s"""$f% discount included""")
      println(ff"""$f%3.2f%% discount included""")
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
      // try { println(s"""Bob is ${ff"$n"} years ${s"$old"}!""") } catch catcher
      // try { println(ff"""Bob is ${s"$n"} years ${s"$old"}!""") } catch catcher
      // try { println(ff"""Bob is ${ff"$n"} years ${s"$old"}!""") } catch catcher
      // try { println(ff"""Bob is ${ff"$n%2d"} years ${s"$old"}!""") } catch catcher
      // try { println(ff"""Bob is ${s"$n%2d"} years ${s"$old"}!""") } catch catcher
      // try { println(s"""Bob is ${ff"$n%2d"} years ${s"$old"}!""") } catch catcher
      try { println(s"""Bob is ${s"$n%2d"} years ${s"$old"}!""") } catch catcher
    }

    test1(1)
    println("===============")
    test1(12)
    println("===============")
    test1(123)
  }

  def main(args: Array[String]): Unit = {
    println(ff"integer: ${5}%d")
    println(ff"string: ${"l"}%s")
    println(ff"${5}%s, ${6}%d, ${"hello"}%s")

    val x = 5
    println(ff"$x%d")
    // println(ff"${x + 1}%d")

    // ported from scalac
    testA
    testB
    testC
  }
}