package dotty.tools.benchmarks.specialization

import org.openjdk.jmh.annotations._
import scala.util.Random

class Functions {
  extension (x: Int)
    inline def times(inline work: Int): Int = {
      var res = 0
      var count = 0
      while count < x do
        res += work
        count += 1
      res
    }

  class ByName {
    def foo(x: => Int): Int = x
  }

  @Benchmark
  def byNameBench(): Int = {
    val a = new ByName
    var list = List(a)
    10000.times { list.head.foo(6) }
  }

  @Benchmark
  def lambdaBench(): Int = {
    val fn = (x: Int) => x + 1
    var list = List(fn)
    10000.times { list.head(2) }
  }

  class Func1[T](fn: T => Int) extends Function1[T, Int] {
    def apply(x: T): Int = fn(x)
  }
  class Fn extends Func1(identity[Int])

  @Benchmark
  def extendFun1Bench(): Int = {
    val a: Function1[Int, Int] = new Fn
    var list = List(a)
    10000.times { list.head(123) }
  }

  class Func2 extends Function2[Int, Int, Int] {
    def apply(i: Int, j: Int) = i + j
  }

  @Benchmark
  def extendFun2Bench(): Int = {
    val a: Function2[Int, Int, Int] = new Func2
    var list = List(a)
    10000.times { list.head(1300, 37) }
  }
}
