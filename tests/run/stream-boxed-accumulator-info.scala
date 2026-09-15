import java.{lang => jl}
import java.{util => ju}

import scala.collection.convert.StreamExtensions.*
import scala.jdk.StreamConverters.*
import scala.jdk.{Accumulator, DoubleAccumulator, IntAccumulator, LongAccumulator}

object Test extends App:
  def assertCompanion[A, C](expected: AnyRef | Null)(info: AccumulatorFactoryInfo[A, C]): Unit =
    assert(info.companion == expected, s"expected companion $expected, got ${info.companion}")

  assertCompanion[Int, IntAccumulator](IntAccumulator)(implicitly[AccumulatorFactoryInfo[Int, IntAccumulator]])
  assertCompanion[Long, LongAccumulator](LongAccumulator)(implicitly[AccumulatorFactoryInfo[Long, LongAccumulator]])
  assertCompanion[Double, DoubleAccumulator](DoubleAccumulator)(implicitly[AccumulatorFactoryInfo[Double, DoubleAccumulator]])
  assertCompanion[jl.Integer, IntAccumulator](IntAccumulator)(implicitly[AccumulatorFactoryInfo[jl.Integer, IntAccumulator]])
  assertCompanion[jl.Long, LongAccumulator](LongAccumulator)(implicitly[AccumulatorFactoryInfo[jl.Long, LongAccumulator]])
  assertCompanion[jl.Double, DoubleAccumulator](DoubleAccumulator)(implicitly[AccumulatorFactoryInfo[jl.Double, DoubleAccumulator]])

  val longs: LongAccumulator = ju.stream.Stream.of(jl.Long.valueOf(1L), jl.Long.valueOf(2L), jl.Long.valueOf(3L)).toScala(Accumulator)
  assert(longs.sum == 6L, s"boxed Long stream sum was ${longs.sum}")

  val doubles: DoubleAccumulator = ju.stream.Stream.of(jl.Double.valueOf(1.5), jl.Double.valueOf(2.5)).toScala(Accumulator)
  assert(doubles.sum == 4.0, s"boxed Double stream sum was ${doubles.sum}")