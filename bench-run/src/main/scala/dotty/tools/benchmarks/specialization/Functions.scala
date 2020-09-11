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

  // outsmart JVM with storage in mutable array
  var byName = new ByName
  var arrByName = Array(byName, null)

  @Benchmark
  def byNameBench(): Int = 10000.times {
    // necessary to outsmart JVM
    // remove it will result in 200x speed up
    arrByName(1) = null
    arrByName(0).foo(6)
  }


  var fn = (x: Int) => x + 1
  var arr = Array(fn, null)
  @Benchmark
  def lambdaBench(): Int = 10000.times {
    arr(1) = null
    arr(0)(2)
  }

  class Func1[T](fn: T => Int) extends Function1[T, Int] {
    def apply(x: T): Int = fn(x)
  }
  class Fn extends Func1(identity[Int])

  var fn1: Function1[Int, Int] = new Fn
  var arr1 = Array(fn1, null)

  @Benchmark
  def extendFun1Bench(): Int = 10000.times {
    arr1(1) = null
    arr1(0)(12)
  }


  class Func2 extends Function2[Int, Int, Int] {
    def apply(i: Int, j: Int) = i + j
  }

  var fn2: Function2[Int, Int, Int] = new Func2
  var arr2 = Array(fn2, null)

  @Benchmark
  def extendFun2Bench(): Int = 10000.times {
    arr2(1) = null
    arr2(0)(1300, 37)
  }
}
