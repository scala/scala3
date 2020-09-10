package dotty.tools.benchmarks.specialization

import org.openjdk.jmh.annotations._
import scala.util.Random


@State(Scope.Benchmark)
class Functions {
  extension (x: Int)
    inline def times(inline work: Int): Int = {
      var res = 0
      var count = 0
      while count < x do
        res += work + 1
        count += 1
      res
    }

  class ByName {
    def foo(x: => Int): Int = x
  }

  var byName = new ByName

  @Benchmark
  def byNameBench(): Int = 10000.times { byName.foo(6) }


  var fn = (x: Int) => x + 1
  @Benchmark
  def lambdaBench(): Int = 10000.times { fn(2) }

  class Func1[T](fn: T => Int) extends Function1[T, Int] {
    def apply(x: T): Int = fn(x)
  }
  class Fn extends Func1(identity[Int])

  var fn1: Function1[Int, Int] = new Fn

  @Benchmark
  def extendFun1Bench(): Int = 10000.times { fn1(12) }


  class Func2 extends Function2[Int, Int, Int] {
    def apply(i: Int, j: Int) = i + j
  }

  var fn2: Function2[Int, Int, Int] = new Func2

  @Benchmark
  def extendFun2Bench(): Int = 1000000.times { fn2(1300, 37) }
}
