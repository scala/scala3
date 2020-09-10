package dotty.tools.benchmarks.specialization

import org.openjdk.jmh.annotations._
import scala.util.Random

class Functions {
  extension (x: Int)
    inline def times(inline op: Unit): Unit = {
      var count = 0
      while count < x do
        op
        count += 1
    }

  class ByName {
    def foo(x: => Int): Int = x
  }

  @Benchmark
  def byNameBench() = 10000.times {
    val a = new ByName
    var list = List(a)
    list.head.foo(6) + 10
  }

  @Benchmark
  def lambdaBench() = 10000.times {
    val fn = (x: Int) => x + 1
    var list = List(fn)
    list.head(2)
  }

  class Func1[T](fn: T => Int) extends Function1[T, Int] {
    def apply(x: T): Int = fn(x)
  }
  class Fn extends Func1(identity[Int])

  @Benchmark
  def extendFun1Bench() = 10000.times {
    val a: Function1[Int, Int] = new Fn
    var list = List(a)
    list.head(123) + 10
  }

  class Func2 extends Function2[Int, Int, Int] {
    def apply(i: Int, j: Int) = i + j
  }

  @Benchmark
  def extendFun2Bench() = 10000.times {
    val a: Function2[Int, Int, Int] = new Func2
    var list = List(a)
    list.head(1300, 37) + 100
  }
}
